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
    NonExhaustiveMatch(Datum),

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
pub enum Term {
    // functions
    Lam(Box<Term>),             // de Bruijn Index
    Builtin(Builtin),

    // expressions
    GlobalVar(Symbol),          // GlobalVar(usize) in future.
    Var(usize),                 // de Bruijn index
    App(Box<Term>, Box<Term>),  // strict App f x
    Match(Box<Term>, Vec<(Pat, Term)>),
    For(Box<Term>, Box<Term>, Box<Term>), // `__for__ init pred next`
    TupleAccess(Box<Term>, usize),  // ex. `p.0`
    FieldAccess(Box<Term>, Label),  // ex. `p.x`

    Let(Box<Term>, Box<Term>),
    LetRec(Box<Term>, Box<Term>),

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

    /// `let f = x in e` ; Short-circuit of `app(lam(e), x)`
    pub fn let_(x: Term, e: Term) -> Term {
        Term::Let(Box::new(x), Box::new(e))
    }

    /// `let rec f = x in e`
    pub fn letrec(x: Term, e: Term) -> Term {
        Term::LetRec(Box::new(x), Box::new(e))
    }

    // ---------------------------------------------------------
    pub fn unit() -> Term {
        Term::Lit(Lit::Unit)
    }
    pub fn bool_(x: bool) -> Term {
        Term::Lit(Lit::Bool(x))
    }
    pub fn int(x: i64) -> Term {
        Term::Lit(Lit::Int(x))
    }

    // ---------------------------------------------------------
    // === syntax sugar ===
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

    pub fn clo(t: Term, args: Vec<Term>) -> Term {
        let mut f = t;
        for _ in 0..args.len() {
            f = Term::lam(f);
        }
        for x in args {
            f = Term::app(f, x);
        }
        f
    }

    /// (\p. e) ; Lambda abstraction that takes a pattern argument.
    pub fn lam_p1(p: Pat, e: Term) -> Term {
        Term::lam(Term::match_(Term::Var(0), vec![(p, e)]))
    }

    /// let p = x in e ; e's env.len() == 1 + |p|
    pub fn let_p1(p: Pat, x: Term, e: Term) -> Term {
        // Term::app(Term::lam_p1(p, e), x)
        Term::let_(x, Term::match_(Term::Var(0), vec![(p, e)]))
    }

    /// let p = x in e ; e's env.len() == |p|
    pub fn let_p0(p: Pat, x: Term, e: Term) -> Term {
        Term::match_(x, vec![(p, e)])
    }

    /// if (cond) t f
    pub fn if_(cond: Term, t: Term, f: Term) -> Term {
        Term::match_(cond, vec![
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
    Con(ConId, Vec<Pat>),       // `Cons x xs`
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Record(Vec<(Label, Pat)>),
}

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    pub term: Term,
    pub env: Env,
}

pub type GlobalEnv = IndexMap<Symbol, Term>; // Vec<Term> in future
type Env = Vec<Addr>;
type Addr = Rc<RefCell<Datum>>;
type AStack = RefStack<Addr>;
type UStack = Vec<(Addr, AStack)>;

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Unit,
    Bool(bool),
    I64(i64),
    Con(Symbol, Vec<Datum>),
    Tuple(Vec<Datum>),
    Record(Vec<Label>, Vec<Datum>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Datum {
    Clo(Closure),
    Val(Value),
}

impl Datum {
    pub fn value(&self) -> &Value {
        match self {
            Datum::Val(v) => v,
            _ => panic!(),
        }
    }
    pub fn term(&self) -> &Term {
        match self {
            Datum::Clo(c) => &c.term,
            _ => panic!(),
        }
    }
    pub fn env(&self) -> &Env {
        match self {
            Datum::Clo(c) => &c.env,
            _ => panic!(),
        }
    }
    pub fn env_at(&self, index: usize) -> Datum {
        self.env()[index].borrow().clone()
    }
}

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct State {
    pub datum: Datum,
    pub args: AStack,
    pub upds: UStack,
}

// -------------------------------------------------------------
pub struct VM<'a> {
    globals: &'a GlobalEnv,
    state: State,
}

impl <'a> VM<'a> {
    pub fn run(globals: &'a GlobalEnv, term: Term) -> Result<Datum, RuntimeError> {
        VM::new(globals, term).eval()
    }
}

impl <'a> VM<'a> {
    pub fn new(globals: &'a GlobalEnv, term: Term) -> Self {
        let state = State {
            datum: Datum::Clo(Closure { term, env: Env::new() }),
            args: AStack::new(),
            upds: UStack::new(),
            // heap: Heap::new(),
        };
        VM { globals, state }
    }

    /// Evaluate the current closure and return the result closure.
    pub fn eval(&mut self) -> Result<Datum, RuntimeError> {
        self.run_state_whnf()?;
        Ok(self.state.datum.clone())
    }
}

// -------------------------------------------------------------
// === VM internal evaluation APIs ===
impl VM<'_> {
    fn run_state_whnf(&mut self) -> Result<(), RuntimeError> {
        loop {
            if let Datum::Val(_) = &self.state.datum {
                return Ok(())
            }
            match self.term() {
                Term::Lit(x) => {
                    // self.env_clear();
                    let val = match x {
                        Lit::Unit => Value::Unit,
                        Lit::Bool(b) => Value::Bool(*b),
                        Lit::Int(i) => Value::I64(*i),
                    };
                    return self.run_state_value(val);
                }
                Term::Con(c, arity) => {
                    return self.run_state_value(Value::Con(c.clone(), self.env_values(*arity)));
                }
                Term::Tuple(arity) => {
                    return self.run_state_value(Value::Tuple(self.env_values(*arity)));
                }
                Term::Record(labels) => {
                    return self.run_state_value(Value::Record(labels.clone(), self.env_values(labels.len())));
                }
                Term::Lam(_) | Term::Builtin(_) if self.args_is_empty() => {
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
        match self.term() {
            Term::GlobalVar(_) => {
                self.run_global_access()
            }
            Term::Var(_) => {
                self.run_access()
            }
            Term::App(_, _) => {
                self.run_app()
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

            Term::Lam(_) if !self.args_is_empty() => {
                self.run_lam();
                Ok(())
            }
            Term::Builtin(_) if !self.args_is_empty() => {
                self.run_builtin()
            }

            Term::Let(_, _) => {
                self.run_let()
            }
            Term::LetRec(_, _) => {
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

    /// Evaluates the given term in the current closure's environment and
    /// returns the resulting value.
    fn eval_term(&mut self, term: Term) -> Result<Datum, RuntimeError> {
        self.term_replace(term);
        self.eval()
    }
}

// -------------------------------------------------------------
// === VM internal APIs ===
impl VM<'_> {
    fn set_value(&mut self, val: Value) {
        self.state.datum = Datum::Val(val);
    }

    fn value(&self) -> &Value {
        self.state.datum.value()
    }

    fn set_closure(&mut self, term: Term, env: Env) {
        self.state.datum = Datum::Clo(Closure { term, env });
    }

    fn term(&self) -> &Term {
        self.state.datum.term()
    }

    fn term_replace(&mut self, term: Term) {
        match &mut self.state.datum {
            Datum::Clo(c) => c.term = term,
            _ => panic!(),
        }
    }

    fn env(&self) -> &Env {
        self.state.datum.env()
    }

    fn env_mut(&mut self) -> &mut Env {
        match &mut self.state.datum {
            Datum::Clo(c) => &mut c.env,
            _ => panic!(),
        }
    }

    fn env_replace(&mut self, env: Env) {
        *self.env_mut() = env;
    }

    fn env_values(&self, arity: usize) -> Vec<Datum> {
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
        let dummy = Datum::Val(Value::Unit);
        alloc(dummy)
    }

    /// Allocate fresh address and store closure
    fn heap_alloc(&self, c: Datum) -> Addr {
        alloc(c)
    }

    /// Load closure from heap.
    fn heap_load(&mut self, a: Addr) {
        self.state.datum = a.borrow().clone();
    }

    /// Store closure to heap.
    fn heap_store(&self, a: Addr) {
        *a.borrow_mut() = self.state.datum.clone();
    }
}

fn alloc(c: Datum) -> Addr {
    Rc::new(RefCell::new(c))
}

fn match_pat(pat: &Pat, val: &Datum) -> Option<Env> {
    let mut env = Env::new();
    match (pat, val) {
        (Pat::Wildcard, _) => Some(env),

        (Pat::Lit(Lit::Unit), Datum::Val(Value::Unit)) => Some(env),
        (Pat::Lit(Lit::Bool(p)), Datum::Val(Value::Bool(x))) if p == x => Some(env),
        (Pat::Lit(Lit::Int(p)), Datum::Val(Value::I64(x))) if p == x => Some(env),

        (Pat::Var, v) => {
            env.push(alloc(v.clone()));
            Some(env)
        }

        (Pat::Con(name_p, args_p), Datum::Val(Value::Con(name_v, args_v)))
            if name_p == name_v && args_p.len() == args_v.len() =>
        {
            for (p, v) in args_p.iter().zip(args_v.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Tuple(pats), Datum::Val(Value::Tuple(vals))) if pats.len() == vals.len() => {
            for (p, v) in pats.iter().zip(vals.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        (Pat::Record(fields), Datum::Val(Value::Record(labels, vals))) => {
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
    //     let Term::App(t1, t2) = self.term().clone() else { panic!() };
    //     let c = Datum::Clo(Closure { term: *t2, env: self.env_dup() });
    //     let a = self.heap_alloc(c);
    //     self.args_push(a);
    //     self.term_replace(*t1);
    // }
    fn run_app(&mut self) -> Result<(), RuntimeError> {
        // strict App f x
        let Term::App(t1, t2) = self.term().clone() else { panic!() };
        let env = self.env_dup();
        let f = self.eval_term(*t1)?;
        self.env_replace(env);
        let x = self.eval_term(*t2)?;
        let a = self.heap_alloc(x);
        self.state.datum = f;
        self.args_push(a);
        Ok(())
    }

    fn run_lam(&mut self) {
        let Term::Lam(e) = self.term().clone() else { panic!() };
        self.term_replace(*e);
        let a = self.args_pop();
        self.env_push(a);
    }

    fn run_let(&mut self) -> Result<(), RuntimeError> {
        let Term::Let(x, e) = self.term().clone() else { panic!() };
        let mut env = self.env_dup();
        let x = self.eval_term(*x)?;
        let a = self.heap_alloc(x);
        env.push(a);
        self.set_closure(*e, env);
        Ok(())
    }

    fn run_letrec(&mut self) -> Result<(), RuntimeError> {
        let Term::LetRec(x, e) = self.term().clone() else { panic!() };
        let mut env = self.env_dup();
        let a = self.heap_alloc_reserved();
        self.env_push(a.clone());
        self.term_replace(*x);
        self.run_state_whnf()?;
        self.heap_store(a.clone());
        env.push(a);
        self.set_closure(*e, env);
        Ok(())
    }

    fn run_access(&mut self) -> Result<(), RuntimeError> {
        let Term::Var(v) = self.term() else { panic!() };
        // ACCESS
        let a = self.env_get(*v)?;
        self.heap_load(a.clone());    // clo <- heap[a]

        if let Datum::Val(_) = self.state.datum {
            // no need to UPDATE
            return Ok(());
        }
        match self.term() {
            Term::Lam(_) | Term::Builtin(_) | Term::Con(_, _) | Term::Tuple(_) | Term::Record(_) => {
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
impl VM<'_> {
    fn run_global_access(&mut self) -> Result<(), RuntimeError> {
        let Term::GlobalVar(v) = self.term() else { panic!() };
        let term = self.globals.get(v)
            .ok_or_else(|| RuntimeError::GlobalVariableNotFound(v.clone()))
            .cloned()?;
        self.set_closure(term, Env::new());
        Ok(())
    }

    fn run_tuple_access(&mut self) -> Result<(), RuntimeError> {
        let Term::TupleAccess(term, index) = self.term().clone() else { panic!() };
        self.term_replace(*term);
        self.run_state_whnf()?;
        if let Value::Con(_, args) = self.value() {
            if args.len() != 1 { panic!() }
            self.state.datum = args[0].clone();
        }
        let Value::Tuple(args) = self.value() else { panic!() };
        self.state.datum = args[index].clone();
        Ok(())
    }

    fn run_field_access(&mut self) -> Result<(), RuntimeError> {
        let Term::FieldAccess(term, label) = self.term().clone() else { panic!() };
        self.term_replace(*term);
        self.run_state_whnf()?;
        if let Value::Con(_, args) = self.value() {
            if args.len() != 1 { panic!() }
            self.state.datum = args[0].clone();
        }
        let Value::Record(fs, args) = self.value() else { panic!() };
        let index = fs.iter().position(|s| *s == label).unwrap();
        self.state.datum = args[index].clone();
        Ok(())
    }

    fn run_match(&mut self) -> Result<(), RuntimeError> {
        let Term::Match(term, arms) = self.term().clone() else { panic!() };
        let mut env = self.env_dup();
        let v_scrut = self.eval_term(*term)?;
        for (pat, body) in arms {
            if let Some(bindings) = match_pat(&pat, &v_scrut) {
                env.extend(bindings);
                self.state.datum = Datum::Clo(Closure { term: body, env });
                return Ok(())
            }
        }
        Err(RuntimeError::NonExhaustiveMatch(v_scrut))
    }

    fn run_for(&mut self) -> Result<(), RuntimeError> {
        let Term::For(init, pred, next) = self.term().clone() else { panic!() };

        let p_env = self.env_dup();
        let n_env = self.env_dup();

        let a = {
            let x = self.eval_term(*init)?;
            self.heap_alloc(x)
        };

        self.set_closure(*pred, p_env);
        let pred = self.eval()?;

        self.set_closure(*next, n_env);
        let next = self.eval()?;

        loop {
            self.state.datum = pred.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            let Value::Bool(b) = self.value() else { panic!() };
            if !b { break; }
            self.state.datum = next.clone();
            self.args_push(a.clone());
            self.run_state_whnf()?;
            self.heap_store(a.clone());
        }
        self.heap_load(a);
        Ok(())
    }

    fn run_builtin(&mut self) -> Result<(), RuntimeError> {
        let Term::Builtin(f) = self.term().clone() else { panic!() };
        let a = self.args_pop();
        self.heap_load(a);
        let x = self.eval()?;
        let v = builtin(f, x)?;
        self.state.datum = v;
        Ok(())
    }
}

// -------------------------------------------------------------
impl Datum {
    fn as_i64_pair(&self) -> (&i64, &i64) {
        if let Value::Tuple(es) = self.value() {
            if let [Datum::Val(Value::I64(a)), Datum::Val(Value::I64(b))] = es.as_slice() {
                return (a, b)
            }
        }
        panic!()
    }
}

fn builtin(f: Builtin, x: Datum) -> Result<Datum, RuntimeError> {
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
    Ok(Datum::Val(val))
}
