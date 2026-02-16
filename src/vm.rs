use std::rc::Rc;
use std::cell::RefCell;
use crate::collection::RefStack;

pub use indexmap::IndexMap;
pub use crate::module::Symbol;
pub use crate::syntax::ast::Lit;
pub use crate::runtime::Builtin;

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeError {
    Fatal,
    DivisionByZero,
    GlobalVariableNotFound(Symbol),
    VariableNotFound(usize),
    DanglingReadPointer,
    DanglingWritePointer,
}

// -------------------------------------------------------------
pub type ConId = Symbol;
pub type Label = String;        // label of record field or region

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Lam(Box<Term>),             // de Bruijn Index
    Builtin(Builtin),
    V(Value),
    E(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    GlobalVar(Symbol),          // GlobalVar(usize) in future.
    Var(usize),                 // de Bruijn index
    App(Box<Term>, Box<Term>),
    StrictLetIn(Box<Term>, Box<Term>),
    Match(Box<Term>, Vec<(Pat, Term)>),
    Block(Box<Term>),                     // `{ ... }`
    For(Box<Term>, Box<Term>, Box<Term>), // `__for__ init pred next`
    Con(ConId, Vec<Term>),
    Tuple(Vec<Term>),
    Record(Vec<(Label, Term)>),
    TupleAccess(Box<Term>, usize),  // ex. `p.0`
    FieldAccess(Box<Term>, Label),  // ex. `p.x`
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Lit(Lit),
    Con(ConId, Vec<Value>),
    Tuple(Vec<Value>),
    Record(Vec<(Label, Value)>),
    Closure(Addr),              // or Closure(Box<Closure>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Wildcard,                   // `_`
    Lit(Lit),                   // `()`, `true`, `1`, etc.
    Var,                        // `x` (unnamed; de Bruijn index)
    Con(ConId, Vec<Pat>),       // `Cons x xs`
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Record(Vec<(String, Pat)>),
}

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub term: Term,
    pub env: Env,
}

pub type GlobalEnv = IndexMap<Symbol, Term>; // Vec<Term> in future
type Env = Vec<Addr>;
type Addr = Rc<RefCell<Closure>>;
type AStack = RefStack<Addr>;
type UStack = Vec<(Addr, AStack)>;

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct State {
    pub clo: Closure,
    pub args: AStack,
    pub upds: UStack,
}

// -------------------------------------------------------------
pub struct VM {
    global: GlobalEnv,
    state: State,
}

impl VM {
    pub fn new(global: GlobalEnv, term: Term) -> Self {
        let state = State {
            clo: Closure { term, env: Env::new() },
            args: AStack::new(),
            upds: UStack::new(),
            // heap: Heap::new(),
        };
        VM { global, state }
    }

    pub fn state(&self) -> State {
        self.state.clone()
    }

    pub fn run(&mut self) -> Result<Term, RuntimeError> {
        self.run_state_whnf()?;
        Ok(self.state.clo.term.clone())
    }
}

// -------------------------------------------------------------
// === VM internal evaluation APIs ===
impl VM {
    fn run_state_whnf(&mut self) -> Result<(), RuntimeError> {
        loop {
            match &self.state.clo.term {
                Term::Lam(_) | Term::Builtin(_) if self.state.args.is_empty() => {
                    if self.state.upds.is_empty() {
                        return Ok(());
                    }
                    else {
                        return Err(RuntimeError::DanglingWritePointer);
                    }
                }
                Term::V(Value::Closure(a)) => {
                    // substite & retry! (Don't "fall through")
                    self.heap_load(a.clone());
                    continue;
                }
                Term::V(_) => {
                    if self.state.upds.is_empty() {
                        return Ok(());
                    }
                    if self.state.args.is_empty() {
                        return Err(RuntimeError::DanglingWritePointer);
                    }
                }
                _ => {}
            }
            self.run_state()?;
        }
    }

    fn run_state(&mut self) -> Result<(), RuntimeError> {
        match &mut self.state.clo.term {
            Term::E(e) => match e {
                Expr::GlobalVar(_) => {
                    self.run_global_access()
                }
                Expr::Var(_) => {
                    self.run_access()
                }
                Expr::App(_, _) => {
                    self.run_app();
                    Ok(())
                }
                Expr::StrictLetIn(_, _) => {
                    self.run_strict_let_in()
                }
                Expr::Match(_, _) => {
                    self.run_match()
                }
                Expr::Block(_) => {
                    self.run_block()
                }
                Expr::For(_, _, _) => {
                    self.run_for()
                }
                Expr::Tuple(_) => {
                    self.run_tuple()
                }
                Expr::Con(_, _) => {
                    self.run_con()
                }
                Expr::Record(_) => {
                    self.run_record()
                }
                Expr::TupleAccess(_, _) => {
                    self.run_tuple_access()
                }
                Expr::FieldAccess(_, _) => {
                    self.run_field_access()
                }
            }
            Term::Lam(_) if !self.state.args.is_empty() => {
                self.run_lam();
                Ok(())
            }
            Term::Builtin(_) if !self.state.args.is_empty() => {
                self.run_builtin()
            }
            _ => {
                if !self.state.upds.is_empty() {
                    self.run_update();
                    Ok(())
                }
                else {
                    Err(RuntimeError::Fatal)
                }
            }
        }
    }

    fn eval_into_value(&mut self) -> Result<Value, RuntimeError> {
        self.run_state_whnf()?;
        match &self.state.clo.term {
            Term::V(val) => {
                Ok(val.clone())
            }
            Term::Lam(_) | Term::Builtin(_) => {
                let c = self.state.clo.clone();
                let a = self.heap_alloc(c);
                Ok(Value::Closure(a))
            }
            Term::E(_) => {
                unreachable!()
            }
        }
    }

    fn eval(&mut self, term: Term) -> Result<Closure, RuntimeError> {
        self.state.clo.term = term;
        self.run_state_whnf()?;
        Ok(self.state.clo.clone())
    }
}

// -------------------------------------------------------------
// === VM internal APIs ===
impl VM {
    /// Lookup pointer of the variable.
    fn env_get(&self, v: usize) -> Result<Addr, RuntimeError> {
        let index = self.state.clo.env.len() - v - 1;
        self.state.clo.env.get(index)
            .ok_or_else(|| {
                RuntimeError::VariableNotFound(v)
            }).cloned()
    }

    /// Make shared clone of Env
    fn env_clone(&self) -> Env {
        self.state.clo.env.clone()
    }

    /// Duplicate Env
    fn env_dup(&self) -> Env {
        // self.state.clo.env.duplicate()
        self.state.clo.env.clone()
    }

    /// Push to AStack
    fn args_push(&self, a: Addr) {
        self.state.args.push(a);
    }

    /// Pop from AStack
    fn args_pop(&mut self) -> Addr {
        self.state.args.pop().unwrap()
    }

    /// Push (a, args) to UStack and clear AStack
    fn upds_push(&mut self, a: Addr) {
        self.state.upds.push((a, self.state.args.clone()));
        self.state.args = AStack::new();
    }

    /// Pop (a, args) from UStack and restore AStack
    fn upds_pop(&mut self) -> Addr {
        let (a, args) = self.state.upds.pop().unwrap();
        self.state.args = args;
        a
    }

    fn stack_save(&mut self) -> (AStack, UStack) {
        let saved = (self.state.args.clone(), self.state.upds.clone());
        self.state.args = AStack::new();
        self.state.upds = UStack::new();
        saved
    }

    fn stack_restore(&mut self, stack: (AStack, UStack)) {
        self.state.args = stack.0;
        self.state.upds = stack.1;
    }

    /// Allocate fresh address and store closure
    fn heap_alloc(&mut self, c: Closure) -> Addr {
        alloc(c)
    }

    /// Load closure from heap.
    fn heap_load(&mut self, a: Addr) {
        self.state.clo = a.borrow().clone();
    }

    /// Store closure to heap.
    fn heap_store(&mut self, a: Addr) {
        *a.borrow_mut() = self.state.clo.clone();
    }
}

fn alloc(c: Closure) -> Addr {
    Rc::new(RefCell::new(c))
}

fn match_pat(pat: &Pat, val: &Value) -> Option<Env> {
    let mut env = Env::new();
    match (pat, val) {
        (Pat::Wildcard, _) => Some(env),

        (Pat::Lit(p), Value::Lit(v)) if p == v => Some(env),

        (Pat::Var, v) => {
            env.push(alloc(Closure { term: Term::V(v.clone()), env: Env::new() }));
            Some(env)
        }

        (Pat::Con(name_p, args_p), Value::Con(name_v, args_v))
            if name_p == name_v && args_p.len() == args_v.len() =>
        {
            for (p, v) in args_p.iter().zip(args_v.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Tuple(pats), Value::Tuple(vals)) if pats.len() == vals.len() => {
            for (p, v) in pats.iter().zip(vals.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Record(fields1), Value::Record(fields2)) => {
            for (fname, p) in fields1 {
                match fields2.iter().find(|(n, _)| n == fname) {
                    Some((_, v)) => {
                        let sub = match_pat(p, v)?;
                        env.extend(sub);
                    }
                    None => return None,
                }
            }
            Some(env)
        }

        _ => None,
    }
}

// -------------------------------------------------------------
// === VM basic state transitions ("lazy Krivine machine", Lang(2007)) ===
impl VM {
    fn run_app(&mut self) {
        let Term::E(Expr::App(t1, t2)) = self.state.clo.term.clone() else { panic!() };
        let c = Closure { term: *t2, env: self.env_dup() };
        let a = self.heap_alloc(c);
        self.args_push(a);
        self.state.clo.term = *t1;
    }

    fn run_lam(&mut self) {
        let Term::Lam(e) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *e;
        let a = self.args_pop();
        self.state.clo.env.push(a);
    }

    fn run_access(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Var(v)) = &self.state.clo.term else { panic!() };
        // ACCESS
        let a = self.env_get(*v)?;
        self.heap_load(a.clone());    // clo <- heap[a]

        match self.state.clo.term {
            Term::Lam(_) | Term::Builtin(_) | Term::V(_) => {
                // no need to UPDATE
                return Ok(());
            }
            _ => {
                // schedule UPDATE
                self.upds_push(a);
                return Ok(())
            }
        }
    }

    fn run_update(&mut self) {
        let a = self.upds_pop();
        self.heap_store(a);      // heap[a] <- clo
    }
}

// === VM extended state transitions ===
impl VM {
    fn run_global_access(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::GlobalVar(v)) = &self.state.clo.term else { panic!() };
        let term = self.global.get(v)
            .ok_or_else(|| RuntimeError::GlobalVariableNotFound(v.clone()))
            .cloned()?;
        self.state.clo.term = term;
        self.state.clo.env = Env::new();
        Ok(())
    }

    fn run_strict_let_in(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::StrictLetIn(t1, t2)) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *t1;
        self.run_state_whnf()?;
        let t2 = Term::Lam(t2);
        let t1 = self.state.clo.term.clone();
        self.state.clo.term = Term::E(Expr::App(Box::new(t2), Box::new(t1)));
        Ok(())
    }

    fn run_con(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Con(name, ts)) = self.state.clo.term.clone() else { panic!() };
        let saved = self.stack_save();
        let env = self.env_clone();
        let mut xs = Vec::new();
        for term in ts {
            self.state.clo.term = term;
            // self.state.clo.env = env.duplicate();
            self.state.clo.env = env.clone();
            xs.push(self.eval_into_value()?);
        }
        self.state.clo.term = Term::V(Value::Con(name, xs));
        self.state.clo.env = env;
        self.stack_restore(saved);
        Ok(())
    }

    fn run_tuple(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Tuple(ts)) = self.state.clo.term.clone() else { panic!() };
        let saved = self.stack_save();
        let env = self.env_clone();
        let mut xs = Vec::new();
        for term in ts {
            self.state.clo.term = term;
            // self.state.clo.env = env.duplicate();
            self.state.clo.env = env.clone();
            xs.push(self.eval_into_value()?);
        }
        self.state.clo.term = Term::V(Value::Tuple(xs));
        self.state.clo.env = env;
        self.stack_restore(saved);
        Ok(())
    }

    fn run_record(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Record(fs)) = self.state.clo.term.clone() else { panic!() };
        let saved = self.stack_save();
        let env = self.env_clone();
        let mut xs = Vec::new();
        for (label, term) in fs {
            self.state.clo.term = term;
            // self.state.clo.env = env.duplicate();
            self.state.clo.env = env.clone();
            xs.push((label, self.eval_into_value()?));
        }
        self.state.clo.term = Term::V(Value::Record(xs));
        self.state.clo.env = env;
        self.stack_restore(saved);
        Ok(())
    }

    fn run_tuple_access(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::TupleAccess(term, index)) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *term;
        self.run_state_whnf()?;
        let Term::V(Value::Tuple(ts)) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = Term::V(ts[index].clone());
        self.state.clo.env = Env::new();
        Ok(())
    }

    fn run_field_access(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::FieldAccess(term, label)) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *term;
        self.run_state_whnf()?;
        let Term::V(Value::Record(fs)) = &self.state.clo.term else { panic!() };
        for (name, val) in fs {
            if name == &label {
                self.state.clo.term = Term::V(val.clone());
                self.state.clo.env = Env::new();
                return Ok(())
            }
        }
        panic!("unknown field `{label}`")
    }

    fn run_match(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Match(term, arms)) = self.state.clo.term.clone() else { panic!() };
        let mut env = self.env_clone();
        self.state.clo.term = *term;
        let v_scrut = self.eval_into_value()?;
        for (pat, body) in arms {
            if let Some(bindings) = match_pat(&pat, &v_scrut) {
                env.extend(bindings);
                self.state.clo.term = body;
                self.state.clo.env = env;
                return Ok(())
            }
        }
        panic!("non-exhaustive match: no pattern matched value {:?}", v_scrut)
    }

    fn run_block(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::Block(term)) = self.state.clo.term.clone() else { panic!() };
        let stack = self.stack_save();
        let env = self.env_clone();
        self.state.clo.term = *term;
        self.run()?;
        self.state.clo.env = env;
        self.stack_restore(stack);
        Ok(())
    }

    fn run_for(&mut self) -> Result<(), RuntimeError> {
        let Term::E(Expr::For(init, pred, next)) = self.state.clo.term.clone() else { panic!() };

        let env = self.env_clone();
        let a = {
            let x = self.eval(*init)?;
            self.heap_alloc(x)
        };
        let pred = self.eval(*pred)?;
        let next = self.eval(*next)?;

        loop {
            self.state.clo = pred.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            let Term::V(Value::Lit(Lit::Bool(b))) = self.state.clo.term else { panic!() };
            if !b { break; }
            self.state.clo = next.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            self.heap_store(a.clone());
        }
        self.heap_load(a);
        self.state.clo.env = env;
        Ok(())
    }

    fn run_builtin(&mut self) -> Result<(), RuntimeError> {
        let Term::Builtin(f) = self.state.clo.term.clone() else { panic!() };
        let a = self.args_pop();
        self.heap_load(a);
        let x = self.eval_into_value()?;
        let v = builtin(f, x)?;
        self.state.clo.term = Term::V(v);
        Ok(())
    }
}

fn builtin(f: Builtin, x: Value) -> Result<Value, RuntimeError> {
    match f {
        // Builtin::BoolNot => {
        //     let Value::Lit(Lit::Bool(b)) = x else { panic!() };
        //     Ok(Value::Lit(Lit::Bool(!b)))
        // }
        Builtin::I64Neg => {
            let Value::Lit(Lit::Int(a)) = x else { panic!() };
            Ok(Value::Lit(Lit::Int(-a)))
        }

        Builtin::I64Eq => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a == b)))
        }
        Builtin::I64Neq => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a != b)))
        }

        Builtin::I64Lt => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a < b)))
        }
        Builtin::I64Le => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a <= b)))
        }
        Builtin::I64Gt => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a > b)))
        }
        Builtin::I64Ge => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Bool(a >= b)))
        }

        Builtin::I64Add => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Int(a + b)))
        }
        Builtin::I64Sub => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Int(a - b)))
        }
        Builtin::I64Mul => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            Ok(Value::Lit(Lit::Int(a * b)))
        }
        Builtin::I64Div => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            if *b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Ok(Value::Lit(Lit::Int(a / b)))
        }
        Builtin::I64Mod => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            if *b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Ok(Value::Lit(Lit::Int(a % b)))
        }
    }
}
