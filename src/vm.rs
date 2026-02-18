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
    // functions
    Lam(Box<Term>),             // de Bruijn Index
    Builtin(Builtin),

    // expressions
    GlobalVar(Symbol),          // GlobalVar(usize) in future.
    Var(usize),                 // de Bruijn index
    App(Box<Term>, Box<Term>),  // strict App f x
    Let(Box<Term>, Box<Term>),  // strict Let x in e
    Match(Box<Term>, Vec<(Pat, Term)>),
    For(Box<Term>, Box<Term>, Box<Term>), // `__for__ init pred next`
    TupleAccess(Box<Term>, usize),  // ex. `p.0`
    FieldAccess(Box<Term>, Label),  // ex. `p.x`

    // values (as closure = (term, env))
    Lit(Lit),
    Con(ConId, usize),              // `Con "Cons" 2`, `Con "Nil" 0`
    Tuple(usize),                   // `Tuple 2`, `Tuple 1`
    Record(Vec<Label>),
}

impl Term {
    pub fn lam(e: Term) -> Term {
        Term::Lam(Box::new(e))
    }
    pub fn app(f: Term, x: Term) -> Term {
        Term::App(Box::new(f), Box::new(x))
    }
    pub fn let_(x: Term, e: Term) -> Term {
        Term::Let(Box::new(x), Box::new(e))
    }
    pub fn match_(scrut: Term, arms: Vec<(Pat, Term)>) -> Term {
        Term::Match(Box::new(scrut), arms)
    }
    pub fn for_(init: Term, pred: Term, next: Term) -> Term {
        Term::For(Box::new(init), Box::new(pred), Box::new(next))
    }
    pub fn tuple_access(t: Term, index: usize) -> Term {
        Term::TupleAccess(Box::new(t), index)
    }
    pub fn field_access(r: Term, label: Label) -> Term {
        Term::FieldAccess(Box::new(r), label)
    }

    pub fn block(mut xs: Vec<Term>) -> Term {
        if xs.is_empty() {
            return Term::unit()
        }

        let mut e = xs.pop().unwrap();
        for x in xs.into_iter().rev() {
            e = Term::let_(x, e);
        }
        e
    }

    pub fn unit() -> Term {
        Term::Lit(Lit::Unit)
    }
    pub fn bool_(x: bool) -> Term {
        Term::Lit(Lit::Bool(x))
    }
    pub fn int(x: i64) -> Term {
        Term::Lit(Lit::Int(x))
    }
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

    pub fn run(&mut self) -> Result<Closure, RuntimeError> {
        self.run_state_whnf()?;
        Ok(self.state.clo.clone())
    }
}

// -------------------------------------------------------------
// === VM internal evaluation APIs ===
impl VM {
    fn trim_env(&mut self, arity: usize) {
        let len = self.state.clo.env.len();
        if len > arity {
            self.state.clo.env = self.state.clo.env.split_off(len - arity);
            // eprintln!("[trim] {} -> {}", len, arity);
        }
    }

    fn run_state_whnf(&mut self) -> Result<(), RuntimeError> {
        loop {
            match &self.state.clo.term {
                Term::Tuple(arity) => self.trim_env(*arity),
                Term::Con(_, arity) => self.trim_env(*arity),
                Term::Record(labels) => self.trim_env(labels.len()),
                _ => {}
            }
            match &self.state.clo.term {
                Term::Lam(_) | Term::Builtin(_) if self.state.args.is_empty() => {
                    if self.state.upds.is_empty() {
                        return Ok(());
                    }
                    else {
                        return Err(RuntimeError::DanglingWritePointer);
                    }
                }
                Term::Lit(_) | Term::Con(_, _) | Term::Tuple(_) | Term::Record(_) => {
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
            Term::GlobalVar(_) => {
                self.run_global_access()
            }
            Term::Var(_) => {
                self.run_access()
            }
            Term::App(_, _) => {
                self.run_app()
            }
            Term::Let(_, _) => {
                self.run_let()
            }
            Term::Match(_, _) => {
                self.run_match()
            }
            Term::For(_, _, _) => {
                self.run_for()
            }
            Term::TupleAccess(_, _) => {
                self.run_tuple_access()
            }
            Term::FieldAccess(_, _) => {
                self.run_field_access()
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

    fn eval_into_value(&mut self) -> Result<Closure, RuntimeError> {
        self.run_state_whnf()?;
        Ok(self.state.clo.clone())
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

    /// Duplicate Env
    fn env_dup(&self) -> Env {
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

fn match_pat(pat: &Pat, val: &Closure) -> Option<Env> {
    let mut env = Env::new();
    match (pat, val) {
        (Pat::Wildcard, _) => Some(env),

        (Pat::Lit(p), Closure{ term: Term::Lit(v), ..}) if p == v => Some(env),

        (Pat::Var, v) => {
            env.push(alloc(v.clone()));
            Some(env)
        }

        (Pat::Con(name_p, args_p), Closure{ term: Term::Con(name_v, arity), env: args_v})
            if name_p == name_v && args_p.len() == *arity =>
        {
            let n = args_v.len() - *arity;
            for (p, v) in args_p.iter().zip(args_v[n..].iter()) {
                let sub = match_pat(p, &v.borrow())?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Tuple(pats), Closure{ term: Term::Tuple(arity), env: vals}) if pats.len() == *arity => {
            let n = vals.len() - *arity;
            for (p, v) in pats.iter().zip(vals[n..].iter()) {
                let sub = match_pat(p, &v.borrow())?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Record(fields1), Closure{ term: Term::Record(labels), env: vals}) => {
            let n = vals.len() - labels.len();
            for (fname, p) in fields1 {
                let i = labels.iter().position(|n| n == fname).unwrap();
                let v = &vals[n+i].borrow();
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        _ => None,
    }
}

// -------------------------------------------------------------
// === VM basic state transitions ("lazy Krivine machine", Lang(2007)) ===
impl VM {
    // fn run_app(&mut self) {
    //     let Term::App(t1, t2) = self.state.clo.term.clone() else { panic!() };
    //     let c = Closure { term: *t2, env: self.env_dup() };
    //     let a = self.heap_alloc(c);
    //     self.args_push(a);
    //     self.state.clo.term = *t1;
    // }
    fn run_app(&mut self) -> Result<(), RuntimeError> {
        // strict App f x
        let Term::App(t1, t2) = self.state.clo.term.clone() else { panic!() };
        let env = self.env_dup();
        let f = self.eval(*t1)?;
        self.state.clo.env = env;
        let x = self.eval(*t2)?;
        let a = self.heap_alloc(x);
        self.state.clo = f;
        self.args_push(a);
        Ok(())
    }

    fn run_lam(&mut self) {
        let Term::Lam(e) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *e;
        let a = self.args_pop();
        self.state.clo.env.push(a);
    }

    fn run_access(&mut self) -> Result<(), RuntimeError> {
        let Term::Var(v) = &self.state.clo.term else { panic!() };
        // ACCESS
        let a = self.env_get(*v)?;
        self.heap_load(a.clone());    // clo <- heap[a]

        match self.state.clo.term {
            Term::Lam(_) | Term::Builtin(_) | Term::Lit(_) | Term::Con(_, _) | Term::Tuple(_) | Term::Record(_) => {
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
        let Term::GlobalVar(v) = &self.state.clo.term else { panic!() };
        let term = self.global.get(v)
            .ok_or_else(|| RuntimeError::GlobalVariableNotFound(v.clone()))
            .cloned()?;
        self.state.clo.term = term;
        self.state.clo.env = Env::new();
        Ok(())
    }

    fn run_let(&mut self) -> Result<(), RuntimeError> {
        let Term::Let(t1, t2) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = Term::app(Term::lam(*t2), *t1);
        Ok(())
    }

    fn run_tuple_access(&mut self) -> Result<(), RuntimeError> {
        let Term::TupleAccess(term, index) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *term;
        self.run_state_whnf()?;
        let Term::Tuple(arity) = self.state.clo.term.clone() else { panic!() };
        let i = self.state.clo.env.len() + index - arity;
        let a = self.state.clo.env[i].clone();
        self.heap_load(a);
        Ok(())
    }

    fn run_field_access(&mut self) -> Result<(), RuntimeError> {
        let Term::FieldAccess(term, label) = self.state.clo.term.clone() else { panic!() };
        self.state.clo.term = *term;
        self.run_state_whnf()?;
        let Term::Record(fs) = &self.state.clo.term else { panic!() };
        let arity = fs.len();
        let index = fs.iter().position(|s| *s == label).unwrap();
        let i = self.state.clo.env.len() + index - arity;
        let a = self.state.clo.env[i].clone();
        self.heap_load(a);
        Ok(())
    }

    fn run_match(&mut self) -> Result<(), RuntimeError> {
        let Term::Match(term, arms) = self.state.clo.term.clone() else { panic!() };
        let mut env = self.env_dup();
        let v_scrut = self.eval(*term)?;
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

    fn run_for(&mut self) -> Result<(), RuntimeError> {
        let Term::For(init, pred, next) = self.state.clo.term.clone() else { panic!() };

        let env = self.env_dup();
        let a = {
            let x = self.eval(*init)?;
            self.heap_alloc(x)
        };

        self.state.clo.env = env.clone();
        let pred = self.eval(*pred)?;

        self.state.clo.env = env;
        let next = self.eval(*next)?;

        loop {
            self.state.clo = pred.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            let Term::Lit(Lit::Bool(b)) = self.state.clo.term else { panic!() };
            if !b { break; }
            self.state.clo = next.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            self.heap_store(a.clone());
        }
        self.heap_load(a);
        Ok(())
    }

    fn run_builtin(&mut self) -> Result<(), RuntimeError> {
        let Term::Builtin(f) = self.state.clo.term.clone() else { panic!() };
        let a = self.args_pop();
        self.heap_load(a);
        let x = self.eval_into_value()?;
        let v = builtin(f, x)?;
        self.state.clo = v;
        Ok(())
    }
}

fn builtin(f: Builtin, x: Closure) -> Result<Closure, RuntimeError> {
    let term = match f {
        Builtin::I64Neg => {
            let Term::Lit(Lit::Int(a)) = x.term else { panic!() };
            Term::Lit(Lit::Int(-a))
        }

        Builtin::I64Eq => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a == b))
        }
        Builtin::I64Neq => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a != b))
        }

        Builtin::I64Lt => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a < b))
        }
        Builtin::I64Le => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a <= b))
        }
        Builtin::I64Gt => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a > b))
        }
        Builtin::I64Ge => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Bool(a >= b))
        }

        Builtin::I64Add => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Int(a + b))
        }
        Builtin::I64Sub => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Int(a - b))
        }
        Builtin::I64Mul => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            Term::Lit(Lit::Int(a * b))
        }
        Builtin::I64Div => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            if b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Term::Lit(Lit::Int(a / b))
        }
        Builtin::I64Mod => {
            let n = x.env.len() - 2;
            let Term::Lit(Lit::Int(a)) = x.env[n+0].borrow().term else { panic!() };
            let Term::Lit(Lit::Int(b)) = x.env[n+1].borrow().term else { panic!() };
            if b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Term::Lit(Lit::Int(a % b))
        }
    };
    Ok(Closure { term, env: Env::new() })
}
