mod display;
pub mod heap;
use heap::{Addr, Buf, Slice, ArrayLike};

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
    IndexOutOfBounds,

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
    IndexAccess(Box<Code>, Box<Code>),    // ex. `p[0]`
    TupleAccess(Box<Code>, usize),  // ex. `p.0`
    FieldAccess(Box<Code>, Label),  // ex. `p.x`

    Let(Box<Code>, Box<Code>),
    LetRec(Box<Code>, Box<Code>),

    // value constructors (`Closure {code, env} -> Value`)
    Lit(Lit),
    Tuple(usize),        // `Tuple 2`, `Tuple 1`
    Con(ConId, usize),   // `Con "Cons" 2`, `Con "Nil" 0`
    Record(Vec<Label>),  // `Record ["x", "y"]`
    Array(usize),        // `Array 2`, `Array 1` ; Constructs slice (`@[a]`) of new array
    ArrayU8(usize),
    ArrayI64(usize),

    // DynArray(Box<Code>), // Constructs mutable reference of new dynamic array (e.g. `buf @[a]`)
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
    pub fn index_access(t: Code, i: Code) -> Code {
        Code::IndexAccess(Box::new(t), Box::new(i))
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
    Array(Vec<Pat>, Option<PatRest>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatRest {
    Any,
    Named,
}

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    pub code: Code,
    pub env: Env,
}

pub type GlobalEnv = IndexMap<Symbol, Code>; // Vec<Code> in future
type Env = Vec<Addr>;
type AStack = RefStack<Addr>;
type UStack = Vec<(Addr, AStack)>;

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Unit,
    Bool(bool),
    I64(i64),
    U8(u8),
    Tuple(Vec<Term>),
    Con(ConId, Vec<Term>),
    Record(Vec<Label>, Vec<Term>),

    Array(Slice<Term>),         // immutable slice `@[a]`
    ArrayU8(Slice<u8>),
    ArrayI64(Slice<i64>),

    DynArray(Buf<Term>),        // mutable reference of dynamic array
}

// -------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Clo(Closure),
    Val(Value),
}

// -------------------------------------------------------------
// === boxing / unboxing ===
// --- arrays ---
impl Into<Value> for Slice<Term> {
    fn into(self) -> Value {
        Value::Array(self)
    }
}

impl Into<Value> for Slice<u8> {
    fn into(self) -> Value {
        Value::ArrayU8(self)
    }
}

impl Into<Value> for Slice<i64> {
    fn into(self) -> Value {
        Value::ArrayI64(self)
    }
}

// --- u8 ---
impl Into<Term> for u8 {
    fn into(self) -> Term {
        Term::Val(Value::U8(self))
    }
}

impl TryFrom<Term> for u8 {
    type Error = RuntimeError;
    fn try_from(value: Term) -> Result<Self, Self::Error> {
        if let Term::Val(Value::U8(i)) = value {
            Ok(i)
        }
        else {
            unreachable!()
        }
    }
}

// --- i64 ---
impl Into<Term> for i64 {
    fn into(self) -> Term {
        Term::Val(Value::I64(self))
    }
}

impl TryFrom<Term> for i64 {
    type Error = RuntimeError;
    fn try_from(value: Term) -> Result<Self, Self::Error> {
        if let Term::Val(Value::I64(i)) = value {
            Ok(i)
        }
        else {
            unreachable!()
        }
    }
}

// -------------------------------------------------------------
impl Term {
    pub fn value(&self) -> &Value {
        match self {
            Term::Val(v) => v,
            _ => unreachable!(),
        }
    }
    pub fn code(&self) -> &Code {
        match self {
            Term::Clo(c) => &c.code,
            _ => unreachable!(),
        }
    }
    pub fn env(&self) -> &Env {
        match self {
            Term::Clo(c) => &c.env,
            _ => unreachable!(),
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
                    let val = match x {
                        Lit::Unit => Value::Unit,
                        Lit::Bool(b) => Value::Bool(*b),
                        Lit::Int(i) => Value::I64(*i),
                        Lit::U8(i) => Value::U8(*i),
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
                Code::Array(arity) => {
                    let s = if *arity == 0 {
                        Slice::Empty
                    }
                    else {
                        let arr = self.heap_array(self.env_values(*arity));
                        Slice::Some { arr, beg: 0, end: *arity }
                    };
                    return self.run_state_value(Value::Array(s));
                }
                Code::ArrayU8(arity) => {
                    let s = if *arity == 0 {
                        Slice::Empty
                    }
                    else {
                        let xs: Vec<_> = self.env_values(*arity).into_iter().map(|t| {
                            let Term::Val(Value::U8(i)) = t else { unreachable!() };
                            i
                        }).collect();
                        let arr = self.heap_array(xs);
                        Slice::Some { arr, beg: 0, end: *arity }
                    };
                    return self.run_state_value(Value::ArrayU8(s));
                }
                Code::ArrayI64(arity) => {
                    let s = if *arity == 0 {
                        Slice::Empty
                    }
                    else {
                        let xs: Vec<_> = self.env_values(*arity).into_iter().map(|t| {
                            let Term::Val(Value::I64(i)) = t else { unreachable!() };
                            i
                        }).collect();
                        let arr = self.heap_array(xs);
                        Slice::Some { arr, beg: 0, end: *arity }
                    };
                    return self.run_state_value(Value::ArrayI64(s));
                }
                Code::Lam(_) if self.args_is_empty() => {
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
            Code::IndexAccess(_, _) => {
                self.run_index_access()
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
            Code::Builtin(_) => {
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
            _ => unreachable!(),
        }
    }

    fn env(&self) -> &Env {
        self.state.term.env()
    }

    fn env_mut(&mut self) -> &mut Env {
        match &mut self.state.term {
            Term::Clo(c) => &mut c.env,
            _ => unreachable!(),
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
        if self.env().len() <= v {
            return Err(RuntimeError::VariableNotFound(v));
        }
        let index = self.env().len() - v - 1;
        let a = Rc::clone(&self.env()[index]);
        Ok(a)
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
        heap::alloc_reserved()
    }

    /// Allocate fresh address and store closure / value
    fn heap_alloc(&self, t: Term) -> Addr {
        heap::alloc(t)
    }

    fn heap_array<T: Clone>(&self, v: Vec<T>) -> Buf<T> {
        heap::array(v)
    }

    /// Load closure / value from heap.
    fn heap_load(&mut self, a: Addr) {
        self.state.term = heap::load(a);
    }

    /// Store closure / value to heap.
    fn heap_store(&self, a: Addr) {
        heap::store(a, &self.state.term);
    }
}

fn match_pat(pat: &Pat, val: &Term) -> Option<Env> {
    let mut env = Env::new();
    match (pat, val) {
        (Pat::Wildcard, _) => Some(env),

        (Pat::Lit(Lit::Unit), Term::Val(Value::Unit)) => Some(env),
        (Pat::Lit(Lit::Bool(p)), Term::Val(Value::Bool(x))) if p == x => Some(env),
        (Pat::Lit(Lit::Int(p)), Term::Val(Value::I64(x))) if p == x => Some(env),
        (Pat::Lit(Lit::U8(p)), Term::Val(Value::U8(x))) if p == x => Some(env),

        (Pat::Var, v) => {
            env.push(heap::alloc(v.clone()));
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

        (p, Term::Val(Value::Array(s))) => {
            match_pat_array(p, s)
        }
        (p, Term::Val(Value::ArrayU8(s))) => {
            match_pat_array(p, s)
        }
        (p, Term::Val(Value::ArrayI64(s))) => {
            match_pat_array(p, s)
        }

        _ => None,
    }
}

fn match_pat_array<T>(p: &Pat, s: &Slice<T>) -> Option<Env>
where
    T: Clone + Into<Term>,
    Slice<T>: Into<Value>,
{
    let mut env = Env::new();
    match p {
        Pat::Array(ps, None) => {
            let p_len = ps.len();
            let v_len = s.len();
            if p_len == v_len {
                if let Slice::Some { arr, beg, end: _ } = s {
                    for i in 0..p_len {
                        let x = arr.borrow()[beg + i].clone();
                        let sub = match_pat(&ps[i], &x.into())?;
                        env.extend(sub);
                    }
                }
                Some(env)
            } else {
                None
            }
        }
        Pat::Array(ps, Some(rest)) => {
            let p_len = ps.len();
            let v_len = s.len();
            if p_len <= v_len {
                if let Slice::Some { arr, beg, end: _ } = s {
                    for i in 0..p_len {
                        let x = arr.borrow()[beg + i].clone();
                        let sub = match_pat(&ps[i], &x.into())?;
                        env.extend(sub);
                    }
                }
                if let PatRest::Named = rest {
                    let tail = s.slice(p_len, v_len);
                    let value = tail.into();
                    env.push(heap::alloc(Term::Val(value)));
                }
                Some(env)
            } else {
                None
            }
        }
        _ => None,
    }
}

// -------------------------------------------------------------
// === VM basic state transitions ("lazy Krivine machine", Lang(2007)) ===
impl VM<'_> {
    // fn run_app(&mut self) {
    //     let Code::App(t1, t2) = self.code().clone() else { unreachable!() };
    //     let c = Term::Clo(Closure { code: *t2, env: self.env_dup() });
    //     let a = self.heap_alloc(c);
    //     self.args_push(a);
    //     self.code_replace(*t1);
    // }
    fn run_app(&mut self) -> Result<(), RuntimeError> {
        // strict App f x
        let Code::App(t1, t2) = self.code().clone() else { unreachable!() };
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
        let Code::Lam(e) = self.code().clone() else { unreachable!() };
        self.code_replace(*e);
        let a = self.args_pop();
        self.env_push(a);
    }

    fn run_let(&mut self) -> Result<(), RuntimeError> {
        let Code::Let(x, e) = self.code().clone() else { unreachable!() };
        let mut env = self.env_dup();
        let x = self.eval_code(*x)?;
        let a = self.heap_alloc(x);
        env.push(a);
        self.set_closure(*e, env);
        Ok(())
    }

    fn run_letrec(&mut self) -> Result<(), RuntimeError> {
        let Code::LetRec(x, e) = self.code().clone() else { unreachable!() };
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
        let Code::Var(v) = self.code() else { unreachable!() };
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
        let Code::GlobalVar(v) = self.code() else { unreachable!() };
        let code = self.globals.get(v)
            .ok_or_else(|| RuntimeError::GlobalVariableNotFound(v.clone()))
            .cloned()?;
        self.set_closure(code, Env::new());
        Ok(())
    }

    // Unwrap if the current term was a "newtype pattern".
    fn run_unwrap(&mut self) {
        if let Value::Con(_, args) = self.value() {
            if args.len() != 1 { unreachable!() }
            self.state.term = args[0].clone();
        }
    }

    fn run_index_access(&mut self) -> Result<(), RuntimeError> {
        let Code::IndexAccess(code, arg) = self.code().clone() else { unreachable!() };
        let env = self.env_dup();
        let Term::Val(Value::I64(index)) = self.eval_code(*arg)? else { unreachable!() };
        self.set_closure(*code, env);
        self.run_state_whnf()?;

        self.run_unwrap();

        match self.value() {
            Value::Array(s) => {
                self.state.term = heap::extract_1(s, index)?;
                Ok(())
            }
            Value::ArrayU8(s) => {
                let x = heap::extract_1(s, index)?;
                self.state.term = x.into();
                Ok(())
            }
            Value::ArrayI64(s) => {
                let x = heap::extract_1(s, index)?;
                self.state.term = x.into();
                Ok(())
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn run_tuple_access(&mut self) -> Result<(), RuntimeError> {
        let Code::TupleAccess(code, index) = self.code().clone() else { unreachable!() };
        self.code_replace(*code);
        self.run_state_whnf()?;

        self.run_unwrap();

        let Value::Tuple(args) = self.value() else { unreachable!() };
        self.state.term = args[index].clone();
        Ok(())
    }

    fn run_field_access(&mut self) -> Result<(), RuntimeError> {
        let Code::FieldAccess(code, label) = self.code().clone() else { unreachable!() };
        self.code_replace(*code);
        self.run_state_whnf()?;

        self.run_unwrap();

        let Value::Record(fs, args) = self.value() else { unreachable!() };
        let index = fs.iter().position(|s| *s == label).unwrap();
        self.state.term = args[index].clone();
        Ok(())
    }

    fn run_match(&mut self) -> Result<(), RuntimeError> {
        let Code::Match(code, arms) = self.code().clone() else { unreachable!() };
        let mut env = self.env_dup();
        let v_scrut = self.eval_code(*code)?;
        for (pat, body) in arms {
            if let Some(bindings) = match_pat(&pat, &v_scrut) {
                env.extend(bindings);
                self.set_closure(body, env);
                return Ok(())
            }
        }
        Err(RuntimeError::NonExhaustiveMatch(v_scrut))
    }

    fn run_for(&mut self) -> Result<(), RuntimeError> {
        let Code::For(init, pred, next) = self.code().clone() else { unreachable!() };

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
            let Value::Bool(b) = self.value() else { unreachable!() };
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
        let Code::Builtin(f) = self.code().clone() else { unreachable!() };
        let v = self.builtin(f)?;
        self.state.term = v;
        Ok(())
    }
}

impl VM<'_> {
    fn env_get_term(&self, i: usize) -> Result<Term, RuntimeError> {
        let a = self.env_get(i)?;
        let x = heap::load(a);
        Ok(x)
    }

    fn env_get_value(&self, i: usize) -> Result<Value, RuntimeError> {
        let x = self.env_get_term(i)?;
        Ok(x.value().clone())
    }

    fn env_get_u8(&self, i: usize) -> Result<u8, RuntimeError> {
        if let Value::U8(v) = self.env_get_value(i)? {
            Ok(v)
        } else {
            unreachable!()
        }
    }

    fn env_get_i64(&self, i: usize) -> Result<i64, RuntimeError> {
        if let Value::I64(v) = self.env_get_value(i)? {
            Ok(v)
        } else {
            unreachable!()
        }
    }

    fn env_get_u8x2(&self) -> Result<(u8, u8), RuntimeError> {
        let a = self.env_get_u8(1)?;
        let b = self.env_get_u8(0)?;
        Ok((a, b))
    }

    fn env_get_i64x2(&self) -> Result<(i64, i64), RuntimeError> {
        let a = self.env_get_i64(1)?;
        let b = self.env_get_i64(0)?;
        Ok((a, b))
    }

    fn builtin(&self, f: Builtin) -> Result<Term, RuntimeError> {
        let val = match f {
            // === cast operators ===
            // --- u8 -> a ---
            Builtin::CastU8toI64 => {
                let a = self.env_get_u8(0)?;
                Value::I64(a as i64)
            }
            // --- i64 -> a ---
            Builtin::CastI64toU8 => {
                let a = self.env_get_i64(0)?;
                Value::U8(a as u8)
            }

            // === unary operators ===
            // --- i64 ---
            Builtin::I64Neg => {
                let a = self.env_get_i64(0)?;
                Value::I64(-a)
            }

            // === binary operators ===
            // --- u8 ---
            Builtin::U8Eq => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a == b)
            }
            Builtin::U8Neq => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a != b)
            }

            Builtin::U8Lt => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a < b)
            }
            Builtin::U8Le => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a <= b)
            }
            Builtin::U8Gt => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a > b)
            }
            Builtin::U8Ge => {
                let (a, b) = self.env_get_u8x2()?;
                Value::Bool(a >= b)
            }

            Builtin::U8Add => {
                let (a, b) = self.env_get_u8x2()?;
                Value::U8(a + b)
            }
            Builtin::U8Sub => {
                let (a, b) = self.env_get_u8x2()?;
                Value::U8(a - b)
            }
            Builtin::U8Mul => {
                let (a, b) = self.env_get_u8x2()?;
                Value::U8(a * b)
            }
            Builtin::U8Div => {
                let (a, b) = self.env_get_u8x2()?;
                if b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Value::U8(a / b)
            }
            Builtin::U8Mod => {
                let (a, b) = self.env_get_u8x2()?;
                if b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Value::U8(a % b)
            }

            // --- i64 ---
            Builtin::I64Eq => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a == b)
            }
            Builtin::I64Neq => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a != b)
            }

            Builtin::I64Lt => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a < b)
            }
            Builtin::I64Le => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a <= b)
            }
            Builtin::I64Gt => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a > b)
            }
            Builtin::I64Ge => {
                let (a, b) = self.env_get_i64x2()?;
                Value::Bool(a >= b)
            }

            Builtin::I64Add => {
                let (a, b) = self.env_get_i64x2()?;
                Value::I64(a + b)
            }
            Builtin::I64Sub => {
                let (a, b) = self.env_get_i64x2()?;
                Value::I64(a - b)
            }
            Builtin::I64Mul => {
                let (a, b) = self.env_get_i64x2()?;
                Value::I64(a * b)
            }
            Builtin::I64Div => {
                let (a, b) = self.env_get_i64x2()?;
                if b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Value::I64(a / b)
            }
            Builtin::I64Mod => {
                let (a, b) = self.env_get_i64x2()?;
                if b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Value::I64(a % b)
            }

            // === arrays ===
            Builtin::Len => {
                let x = &self.env_get_value(0)?;
                let len = match x {
                    Value::Array(s) => s.len(),
                    Value::ArrayU8(s) => s.len(),
                    Value::ArrayI64(s) => s.len(),
                    _ => unreachable!(),
                };
                Value::I64(len as i64)
            }
            Builtin::Slice => {
                let xs = self.env_get_value(2)?;
                let i = self.env_get_i64(1)?;
                let j = self.env_get_i64(0)?;
                match xs {
                    Value::Array(s) => {
                        if !(0 <= i && i <= j && j <= s.len() as i64) {
                            return Err(RuntimeError::IndexOutOfBounds);
                        }
                        s.slice(i as usize, j as usize).into()
                    }
                    Value::ArrayU8(s) => {
                        if !(0 <= i && i <= j && j <= s.len() as i64) {
                            return Err(RuntimeError::IndexOutOfBounds);
                        }
                        s.slice(i as usize, j as usize).into()
                    }
                    Value::ArrayI64(s) => {
                        if !(0 <= i && i <= j && j <= s.len() as i64) {
                            return Err(RuntimeError::IndexOutOfBounds);
                        }
                        s.slice(i as usize, j as usize).into()
                    }
                    _ => unreachable!(),
                }
            }
            Builtin::Push => {
                let xs = self.env_get_value(1)?;
                let x = self.env_get_term(0)?;
                match xs {
                    Value::Array(s) => {
                        Value::Array(heap::push(s, x))
                    }
                    Value::ArrayU8(s) => {
                        let x = x.try_into()?;
                        Value::ArrayU8(heap::push(s, x))
                    }
                    Value::ArrayI64(s) => {
                        let x = x.try_into()?;
                        Value::ArrayI64(heap::push(s, x))
                    }
                    _ => unreachable!(),
                }
            }
        };
        Ok(Term::Val(val))
    }
}
