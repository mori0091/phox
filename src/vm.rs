mod display;

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
    // ---- Runtime errors that may be caused by a programming error.
    DivisionByZero,
    NonExhaustiveMatch(Term),

    // ---- Runtime errors that shouldn't normally occur.
    Fatal,
    GlobalVariableNotFound(Symbol),
    VariableNotFound(usize),
    DanglingReadPointer,
    DanglingWritePointer,
}

// -------------------------------------------------------------
pub type ConId = Symbol;
pub type Label = String;        // label of record field or region

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Code {
    // functions
    Lam(Box<Code>),             // de Bruijn Index
    Builtin(Builtin),

    // expressions
    GlobalVar(Symbol),          // GlobalVar(usize) in future.
    Var(usize),                 // de Bruijn index
    App(Box<Code>, Box<Code>),  // strict App f x
    Match(Box<Code>, Vec<(Pat, Code)>),
    For(Box<Code>, Box<Code>, Box<Code>), // `__for__ init pred next`
    TupleAccess(Box<Code>, usize),  // ex. `p.0`
    FieldAccess(Box<Code>, Label),  // ex. `p.x`

    Let(Box<Code>, Box<Code>),
    LetRec(Box<Code>, Box<Code>),

    // values (as closure = (term, env))
    Lit(Lit),
    Tuple(usize),                   // `Tuple 2`, `Tuple 1`
    Con(ConId, usize),              // `Con "Cons" 2`, `Con "Nil" 0`
    Record(Vec<Label>),
}

impl Code {
    pub fn lam(e: Code) -> Code {
        Code::Lam(Box::new(e))
    }
    pub fn app(f: Code, x: Code) -> Code {
        Code::App(Box::new(f), Box::new(x))
    }
    pub fn match_(scrut: Code, arms: Vec<(Pat, Code)>) -> Code {
        Code::Match(Box::new(scrut), arms)
    }
    pub fn for_(init: Code, pred: Code, next: Code) -> Code {
        Code::For(Box::new(init), Box::new(pred), Box::new(next))
    }
    pub fn tuple_access(t: Code, index: usize) -> Code {
        Code::TupleAccess(Box::new(t), index)
    }
    pub fn field_access(r: Code, label: Label) -> Code {
        Code::FieldAccess(Box::new(r), label)
    }

    /// `let f = x in e` ; Short-circuit of `app(lam(e), x)`
    pub fn let_(x: Code, e: Code) -> Code {
        Code::Let(Box::new(x), Box::new(e))
    }

    /// `let rec f = x in e`
    pub fn letrec(x: Code, e: Code) -> Code {
        Code::LetRec(Box::new(x), Box::new(e))
    }

    // ---------------------------------------------------------
    pub fn unit() -> Code {
        Code::Lit(Lit::Unit)
    }
    pub fn bool_(x: bool) -> Code {
        Code::Lit(Lit::Bool(x))
    }
    pub fn int(x: i64) -> Code {
        Code::Lit(Lit::Int(x))
    }

    // ---------------------------------------------------------
    // === syntax sugar ===
    pub fn block(mut xs: Vec<Code>) -> Code {
        if xs.is_empty() {
            return Code::unit()
        }

        let mut e = xs.pop().unwrap();
        for x in xs.into_iter().rev() {
            e = Code::let_(x, e);
        }
        e
    }

    pub fn clo(t: Code, args: Vec<Code>) -> Code {
        let mut f = t;
        for _ in 0..args.len() {
            f = Code::lam(f);
        }
        for x in args {
            f = Code::app(f, x);
        }
        f
    }

    /// (\p. e) ; Lambda abstraction that takes a pattern argument.
    pub fn lam_p1(p: Pat, e: Code) -> Code {
        Code::lam(Code::match_(Code::Var(0), vec![(p, e)]))
    }

    /// let p = x in e ; e's env.len() == 1 + |p|
    pub fn let_p1(p: Pat, x: Code, e: Code) -> Code {
        Code::let_(x, Code::match_(Code::Var(0), vec![(p, e)]))
    }

    /// let p = x in e ; e's env.len() == |p|
    pub fn let_p0(p: Pat, x: Code, e: Code) -> Code {
        Code::match_(x, vec![(p, e)])
    }

    /// if (cond) t f
    pub fn if_(cond: Code, t: Code, f: Code) -> Code {
        Code::match_(cond, vec![
            (Pat::Lit(Lit::Bool(true)), t),
            (Pat::Wildcard, f),
        ])
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pat {
    Wildcard,                   // `_`
    Lit(Lit),                   // `()`, `true`, `1`, etc.
    Var,                        // `x` (unnamed; de Bruijn index)
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Con(ConId, Vec<Pat>),       // `Cons x xs`
    Record(Vec<(Label, Pat)>),
}

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    pub code: Code,
    pub env: Env,
}

pub type GlobalEnv = IndexMap<Symbol, Code>; // Vec<Code> in future
type Env = Vec<Addr>;
type Addr = Rc<RefCell<Term>>;
type AStack = RefStack<Addr>;
type UStack = Vec<(Addr, AStack)>;

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Unit,
    Bool(bool),
    I64(i64),
    Tuple(Vec<Term>),
    Con(ConId, Vec<Term>),
    Record(Vec<Label>, Vec<Term>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Clo(Closure),
    Val(Value),
}

impl Term {
    pub fn value(&self) -> &Value {
        match self {
            Term::Val(v) => v,
            _ => panic!(),
        }
    }
    pub fn code(&self) -> &Code {
        match self {
            Term::Clo(c) => &c.code,
            _ => panic!(),
        }
    }
    pub fn env(&self) -> &Env {
        match self {
            Term::Clo(c) => &c.env,
            _ => panic!(),
        }
    }
    pub fn env_at(&self, index: usize) -> Term {
        self.env()[index].borrow().clone()
    }
}

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct State {
    pub term: Term,
    pub args: AStack,
    pub upds: UStack,
}

// -------------------------------------------------------------
pub struct VM<'a> {
    globals: &'a GlobalEnv,
    state: State,
}

impl <'a> VM<'a> {
    pub fn run(globals: &'a GlobalEnv, code: Code) -> Result<Term, RuntimeError> {
        VM::new(globals, code).eval()
    }
}

impl <'a> VM<'a> {
    pub fn new(globals: &'a GlobalEnv, code: Code) -> Self {
        let state = State {
            term: Term::Clo(Closure { code, env: Env::new() }),
            args: AStack::new(),
            upds: UStack::new(),
            // heap: Heap::new(),
        };
        VM { globals, state }
    }

    /// Evaluate the current closure and return the result closure.
    pub fn eval(&mut self) -> Result<Term, RuntimeError> {
        self.run_state_whnf()?;
        Ok(self.state.term.clone())
    }
}

// -------------------------------------------------------------
// === VM internal evaluation APIs ===
impl VM<'_> {
    fn run_state_whnf(&mut self) -> Result<(), RuntimeError> {
        loop {
            if let Term::Val(_) = &self.state.term {
                return Ok(())
            }
            match self.code() {
                Code::Lit(x) => {
                    // self.env_clear();
                    let val = match x {
                        Lit::Unit => Value::Unit,
                        Lit::Bool(b) => Value::Bool(*b),
                        Lit::Int(i) => Value::I64(*i),
                    };
                    return self.run_state_value(val);
                }
                Code::Con(c, arity) => {
                    return self.run_state_value(Value::Con(c.clone(), self.env_values(*arity)));
                }
                Code::Tuple(arity) => {
                    return self.run_state_value(Value::Tuple(self.env_values(*arity)));
                }
                Code::Record(labels) => {
                    return self.run_state_value(Value::Record(labels.clone(), self.env_values(labels.len())));
                }
                Code::Lam(_) | Code::Builtin(_) if self.args_is_empty() => {
                    if self.upds_is_empty() {
                        return Ok(());
                    }
                    else {
                        return Err(RuntimeError::DanglingWritePointer);
                    }
                }
                _ => {}
            }
            self.run_state()?;
        }
    }

    fn run_state_value(&mut self, val: Value) -> Result<(), RuntimeError> {
        self.set_value(val);
        if self.upds_is_empty() {
            return Ok(());
        }
        if self.args_is_empty() {
            return Err(RuntimeError::DanglingWritePointer);
        }
        self.run_update();
        Ok(())
    }

    fn run_state(&mut self) -> Result<(), RuntimeError> {
        match self.code() {
            Code::GlobalVar(_) => {
                self.run_global_access()
            }
            Code::Var(_) => {
                self.run_access()
            }
            Code::App(_, _) => {
                self.run_app()
            }
            Code::Match(_, _) => {
                self.run_match()
            }
            Code::For(_, _, _) => {
                self.run_for()
            }
            Code::TupleAccess(_, _) => {
                self.run_tuple_access()
            }
            Code::FieldAccess(_, _) => {
                self.run_field_access()
            }

            Code::Lam(_) if !self.args_is_empty() => {
                self.run_lam();
                Ok(())
            }
            Code::Builtin(_) if !self.args_is_empty() => {
                self.run_builtin()
            }

            Code::Let(_, _) => {
                self.run_let()
            }
            Code::LetRec(_, _) => {
                self.run_letrec()
            }

            _ => {
                if !self.upds_is_empty() {
                    self.run_update();
                    Ok(())
                }
                else {
                    Err(RuntimeError::Fatal)
                }
            }
        }
    }

    /// Evaluates the given code in the current closure's environment and
    /// returns the resulting value.
    fn eval_code(&mut self, code: Code) -> Result<Term, RuntimeError> {
        self.code_replace(code);
        self.eval()
    }
}

// -------------------------------------------------------------
// === VM internal APIs ===
impl VM<'_> {
    fn set_value(&mut self, val: Value) {
        self.state.term = Term::Val(val);
    }

    fn value(&self) -> &Value {
        self.state.term.value()
    }

    fn set_closure(&mut self, code: Code, env: Env) {
        self.state.term = Term::Clo(Closure { code, env });
    }

    fn code(&self) -> &Code {
        self.state.term.code()
    }

    fn code_replace(&mut self, code: Code) {
        match &mut self.state.term {
            Term::Clo(c) => c.code = code,
            _ => panic!(),
        }
    }

    fn env(&self) -> &Env {
        self.state.term.env()
    }

    fn env_mut(&mut self) -> &mut Env {
        match &mut self.state.term {
            Term::Clo(c) => &mut c.env,
            _ => panic!(),
        }
    }

    fn env_replace(&mut self, env: Env) {
        *self.env_mut() = env;
    }

    fn env_values(&self, arity: usize) -> Vec<Term> {
        let env = self.env();
        let idx = env.len() - arity;
        let vals: Vec<_> = env[idx..].iter().map(|a| a.borrow().clone()).collect();
        vals
    }

    fn env_push(&mut self, a: Addr) {
        self.env_mut().push(a);
    }

    /// Lookup pointer of the variable.
    fn env_get(&self, v: usize) -> Result<Addr, RuntimeError> {
        let index = self.env().len() - v - 1;
        self.env().get(index)
            .ok_or_else(|| {
                RuntimeError::VariableNotFound(v)
            }).cloned()
    }

    /// Duplicate Env
    fn env_dup(&self) -> Env {
        self.env().clone()
    }

    fn args_is_empty(&self) -> bool {
        self.state.args.is_empty()
    }

    /// Push to AStack
    fn args_push(&self, a: Addr) {
        self.state.args.push(a);
    }

    /// Pop from AStack
    fn args_pop(&mut self) -> Addr {
        self.state.args.pop().unwrap()
    }

    fn upds_is_empty(&self) -> bool {
        self.state.upds.is_empty()
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

    /// Allocate fresh address
    fn heap_alloc_reserved(&self) -> Addr {
        let dummy = Term::Val(Value::Unit);
        alloc(dummy)
    }

    /// Allocate fresh address and store closure / value
    fn heap_alloc(&self, c: Term) -> Addr {
        alloc(c)
    }

    /// Load closure / value from heap.
    fn heap_load(&mut self, a: Addr) {
        self.state.term = a.borrow().clone();
    }

    /// Store closure / value to heap.
    fn heap_store(&self, a: Addr) {
        *a.borrow_mut() = self.state.term.clone();
    }
}

fn alloc(c: Term) -> Addr {
    Rc::new(RefCell::new(c))
}

fn match_pat(pat: &Pat, val: &Term) -> Option<Env> {
    let mut env = Env::new();
    match (pat, val) {
        (Pat::Wildcard, _) => Some(env),

        (Pat::Lit(Lit::Unit), Term::Val(Value::Unit)) => Some(env),
        (Pat::Lit(Lit::Bool(p)), Term::Val(Value::Bool(x))) if p == x => Some(env),
        (Pat::Lit(Lit::Int(p)), Term::Val(Value::I64(x))) if p == x => Some(env),

        (Pat::Var, v) => {
            env.push(alloc(v.clone()));
            Some(env)
        }

        (Pat::Con(name_p, args_p), Term::Val(Value::Con(name_v, args_v)))
            if name_p == name_v && args_p.len() == args_v.len() =>
        {
            for (p, v) in args_p.iter().zip(args_v.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Tuple(pats), Term::Val(Value::Tuple(vals))) if pats.len() == vals.len() => {
            for (p, v) in pats.iter().zip(vals.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Record(fields), Term::Val(Value::Record(labels, vals))) => {
            for (fname, p) in fields {
                let i = labels.iter().position(|n| n == fname).unwrap();
                let v = &vals[i];
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
impl VM<'_> {
    // fn run_app(&mut self) {
    //     let Code::App(t1, t2) = self.code().clone() else { panic!() };
    //     let c = Term::Clo(Closure { code: *t2, env: self.env_dup() });
    //     let a = self.heap_alloc(c);
    //     self.args_push(a);
    //     self.code_replace(*t1);
    // }
    fn run_app(&mut self) -> Result<(), RuntimeError> {
        // strict App f x
        let Code::App(t1, t2) = self.code().clone() else { panic!() };
        let env = self.env_dup();
        let f = self.eval_code(*t1)?;
        self.env_replace(env);
        let x = self.eval_code(*t2)?;
        let a = self.heap_alloc(x);
        self.state.term = f;
        self.args_push(a);
        Ok(())
    }

    fn run_lam(&mut self) {
        let Code::Lam(e) = self.code().clone() else { panic!() };
        self.code_replace(*e);
        let a = self.args_pop();
        self.env_push(a);
    }

    fn run_let(&mut self) -> Result<(), RuntimeError> {
        let Code::Let(x, e) = self.code().clone() else { panic!() };
        let mut env = self.env_dup();
        let x = self.eval_code(*x)?;
        let a = self.heap_alloc(x);
        env.push(a);
        self.set_closure(*e, env);
        Ok(())
    }

    fn run_letrec(&mut self) -> Result<(), RuntimeError> {
        let Code::LetRec(x, e) = self.code().clone() else { panic!() };
        let mut env = self.env_dup();
        let a = self.heap_alloc_reserved();
        self.env_push(a.clone());
        self.code_replace(*x);
        self.run_state_whnf()?;
        self.heap_store(a.clone());
        env.push(a);
        self.set_closure(*e, env);
        Ok(())
    }

    fn run_access(&mut self) -> Result<(), RuntimeError> {
        let Code::Var(v) = self.code() else { panic!() };
        // ACCESS
        let a = self.env_get(*v)?;
        self.heap_load(a.clone()); // term <- heap[a]

        match &self.state.term {
            Term::Val(_) => {
                // no need to UPDATE
                Ok(())
            }
            Term::Clo(c) => match c.code {
                Code::Lam(_)     |
                Code::Builtin(_) |
                Code::Tuple(_)   |
                Code::Con(_, _)  |
                Code::Record(_) => {
                    // no need to UPDATE
                    Ok(())
                }
                _ => {
                    // schedule UPDATE
                    self.upds_push(a);
                    Ok(())
                }
            }
        }
    }

    fn run_update(&mut self) {
        let a = self.upds_pop();
        self.heap_store(a);     // heap[a] <- term
    }
}

// === VM extended state transitions ===
impl VM<'_> {
    fn run_global_access(&mut self) -> Result<(), RuntimeError> {
        let Code::GlobalVar(v) = self.code() else { panic!() };
        let term = self.globals.get(v)
            .ok_or_else(|| RuntimeError::GlobalVariableNotFound(v.clone()))
            .cloned()?;
        self.set_closure(term, Env::new());
        Ok(())
    }

    // Unwrap if the current term was a "newtype pattern".
    fn run_unwrap(&mut self) {
        if let Value::Con(_, args) = self.value() {
            if args.len() != 1 { panic!() }
            self.state.term = args[0].clone();
        }
    }

    fn run_tuple_access(&mut self) -> Result<(), RuntimeError> {
        let Code::TupleAccess(code, index) = self.code().clone() else { panic!() };
        self.code_replace(*code);
        self.run_state_whnf()?;

        self.run_unwrap();

        let Value::Tuple(args) = self.value() else { panic!() };
        self.state.term = args[index].clone();
        Ok(())
    }

    fn run_field_access(&mut self) -> Result<(), RuntimeError> {
        let Code::FieldAccess(code, label) = self.code().clone() else { panic!() };
        self.code_replace(*code);
        self.run_state_whnf()?;

        self.run_unwrap();

        let Value::Record(fs, args) = self.value() else { panic!() };
        let index = fs.iter().position(|s| *s == label).unwrap();
        self.state.term = args[index].clone();
        Ok(())
    }

    fn run_match(&mut self) -> Result<(), RuntimeError> {
        let Code::Match(code, arms) = self.code().clone() else { panic!() };
        let mut env = self.env_dup();
        let v_scrut = self.eval_code(*code)?;
        for (pat, body) in arms {
            if let Some(bindings) = match_pat(&pat, &v_scrut) {
                env.extend(bindings);
                self.state.term = Term::Clo(Closure { code: body, env });
                return Ok(())
            }
        }
        Err(RuntimeError::NonExhaustiveMatch(v_scrut))
    }

    fn run_for(&mut self) -> Result<(), RuntimeError> {
        let Code::For(init, pred, next) = self.code().clone() else { panic!() };

        let p_env = self.env_dup();
        let n_env = self.env_dup();

        let a = {
            let x = self.eval_code(*init)?;
            self.heap_alloc(x)
        };

        self.set_closure(*pred, p_env);
        let pred = self.eval()?;

        self.set_closure(*next, n_env);
        let next = self.eval()?;

        loop {
            self.state.term = pred.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            let Value::Bool(b) = self.value() else { panic!() };
            if !b { break; }
            self.state.term = next.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            self.heap_store(a.clone());
        }
        self.heap_load(a);
        Ok(())
    }

    fn run_builtin(&mut self) -> Result<(), RuntimeError> {
        let Code::Builtin(f) = self.code().clone() else { panic!() };
        let a = self.args_pop();
        self.heap_load(a);
        let x = self.eval()?;
        let v = builtin(f, x)?;
        self.state.term = v;
        Ok(())
    }
}

// -------------------------------------------------------------
impl Term {
    fn as_i64_pair(&self) -> (&i64, &i64) {
        if let Value::Tuple(es) = self.value() {
            if let [Term::Val(Value::I64(a)), Term::Val(Value::I64(b))] = es.as_slice() {
                return (a, b)
            }
        }
        panic!()
    }
}

fn builtin(f: Builtin, x: Term) -> Result<Term, RuntimeError> {
    let val = match f {
        Builtin::I64Neg => {
            let Value::I64(a) = x.value() else { panic!() };
            Value::I64(-a)
        }

        Builtin::I64Eq => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a == b)
        }
        Builtin::I64Neq => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a != b)
        }

        Builtin::I64Lt => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a < b)
        }
        Builtin::I64Le => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a <= b)
        }
        Builtin::I64Gt => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a > b)
        }
        Builtin::I64Ge => {
            let (a, b) = x.as_i64_pair();
            Value::Bool(a >= b)
        }

        Builtin::I64Add => {
            let (a, b) = x.as_i64_pair();
            Value::I64(a + b)
        }
        Builtin::I64Sub => {
            let (a, b) = x.as_i64_pair();
            Value::I64(a - b)
        }
        Builtin::I64Mul => {
            let (a, b) = x.as_i64_pair();
            Value::I64(a * b)
        }
        Builtin::I64Div => {
            let (a, b) = x.as_i64_pair();
            if *b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Value::I64(a / b)
        }
        Builtin::I64Mod => {
            let (a, b) = x.as_i64_pair();
            if *b == 0 {
                return Err(RuntimeError::DivisionByZero);
            }
            Value::I64(a % b)
        }
    };
    Ok(Term::Val(val))
}
