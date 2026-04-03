use super::*;
use std::fmt;

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ---- Runtime errors that may be caused by a programming error.
            RuntimeError::DivisionByZero =>
                write!(f, "division by zero"),
            RuntimeError::NonExhaustiveMatch(clo) =>
                write!(f, "non-exhaustive match: no pattern matched value `{}`", clo),

            // ---- Runtime errors that shouldn't normally occur.
            RuntimeError::Fatal =>
                write!(f, "fatal error"),
            RuntimeError::GlobalVariableNotFound(sym) =>
                write!(f, "global variable not found: `{}`", sym),
            RuntimeError::VariableNotFound(index) =>
                write!(f, "local variable not found: `?{}`", index),
            RuntimeError::DanglingReadPointer =>
                write!(f, "read from dangling pointer"),
            RuntimeError::DanglingWritePointer =>
                write!(f, "write to dangling pointer"),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Var => write!(f, "?"),
            Pat::Wildcard => write!(f, "_"),
            Pat::Lit(x) => write!(f, "{}", x),
            Pat::Con(c, xs) => {
                let mut ys = Vec::with_capacity(1 + xs.len());
                ys.push(c.pretty());
                for x in xs {
                    match x {
                        Pat::Con(_, _) => ys.push(format!("({})", x)),
                        _ => ys.push(format!("{}", x)),
                    }
                }
                write!(f, "{}", ys.join(" "))
            }
            Pat::Tuple(xs) => {
                match xs.len() {
                    0 => unreachable!(),
                    1 => write!(f, "({},)", xs[0]),
                    _ => {
                        let ys: Vec<_> = xs.iter().map(|x| x.to_string()).collect();
                        write!(f, "({})", ys.join(", "))
                    }
                }
            }
            Pat::Record(fs) => {
                let mut ys = Vec::with_capacity(fs.len());
                for (label, pat) in fs {
                    ys.push(format!("{} = {}", label, pat));
                }
                write!(f, "@{{{}}}", ys.join(", "))
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::I64(i) => write!(f, "{i}"),
            Value::Con(c, args) => {
                let mut xs = Vec::with_capacity(1 + args.len());
                xs.push(c.pretty());
                for arg in args {
                    match arg {
                        Datum::Clo(_) => {
                            xs.push(format!("({})", arg));
                        }
                        Datum::Val(Value::Con(_, ys)) if ys.len() > 0 => {
                            xs.push(format!("({})", arg));
                        }
                        _ => {
                            xs.push(format!("{}", arg));
                        }
                    }
                }
                write!(f, "{}", xs.join(" "))
            }
            Value::Tuple(args) => {
                match args.len() {
                    0 => unreachable!(),
                    1 => write!(f, "({},)", args[0]),
                    n => {
                        let mut xs = Vec::with_capacity(n);
                        for arg in args {
                            xs.push(format!("{}", arg));
                        }
                        write!(f, "({})", xs.join(", "))
                    }
                }
            }
            Value::Record(ix, args) => {
                match ix.len() {
                    0 => write!(f, "@{{}}"),
                    n => {
                        let mut xs = Vec::with_capacity(n);
                        for (label, val) in ix.iter().zip(args.iter()) {
                            xs.push(format!("{} = {}", label, val));
                        }
                        write!(f, "@{{ {} }}", xs.join(", "))
                    }
                }
            }
        }
    }
}

impl fmt::Display for Datum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Datum::Clo(c) => write!(f, "{}", c),
            Datum::Val(v) => write!(f, "{}", v),
        }
    }
}

impl Term {
    fn enclose(&self) -> String {
        match self {
            Term::App(_, _)    |
            Term::Lam(_)       |
            Term::Match(_, _)  |
            Term::For(_, _, _) |
            Term ::Let(_, _)   |
            Term::LetRec(_, _) => {
                format!("({})", self)
            }
            _ => {
                format!("{}", self)
            }
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Lam(e) => {
                write!(f, "λ.{}", e)
            }
            Term::Builtin(b) => {
                write!(f, "<builtin({:?})>", b)
            }
            Term::GlobalVar(s) => {
                write!(f, "{}", s.pretty())
            }
            Term::Var(v) => {
                write!(f, "?{}", v)
            }
            Term::App(fun, x) => {
                match **fun {
                    Term::App(_, _) => {
                        write!(f, "{} {}", fun, x.enclose())
                    }
                    _ => {
                        write!(f, "{} {}", fun.enclose(), x.enclose())
                    }
                }
            }
            Term::Match(scrut, arms) => {
                let mut xs = Vec::with_capacity(arms.len());
                for (p, e) in arms {
                    xs.push(format!("  {} => {}", p, e));
                }
                write!(f, "match ({}) {{\n{}\n}}", scrut, xs.join(",\n"))
            }
            Term::For(i, p, n) => {
                write!(f, "__for__ ({}; {}; {})", i, p, n)
            }
            Term::TupleAccess(t, i) => {
                write!(f, "{}.{}", t.enclose(), i)
            }
            Term::FieldAccess(r, i) => {
                write!(f, "{}.{}", r.enclose(), i)
            }
            Term::Let(x, e) => {
                write!(f, "let ? = {} in\n{}", x, e)
            }
            Term::LetRec(x, e) => {
                write!(f, "let rec ? = {} in\n{}", x, e)
            }
            Term::Lit(x) => {
                write!(f, "{}", x)
            }
            Term::Con(c, arity) => {
                write!(f, "<Con({}, {})>", c.pretty(), arity)
            }
            Term::Tuple(arity) => {
                write!(f, "<Tuple({})>", arity)
            }
            Term::Record(ix) => {
                write!(f, "<Record({})>", ix.join(", "))
            }
        }
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.term {
            Term::Con(c, arity) => {
                let mut xs = Vec::with_capacity(1 + arity);
                xs.push(c.pretty());
                for arg in self.env.iter() {
                    let x = arg.borrow().clone();
                    match &x {
                        Datum::Val(v) => {
                            xs.push(format!("{}", v));
                        }
                        Datum::Clo(c) => match &c.term {
                            Term::Con(_, arity) if *arity > 0 => {
                                xs.push(format!("({})", x));
                            }
                            _ => {
                                xs.push(format!("{}", x));
                            }
                        }
                    }
                }
                write!(f, "{}", xs.join(" "))
            }
            Term::Tuple(arity) => {
                match arity {
                    0 => unreachable!(),
                    1 => write!(f, "({},)", self.env[0].borrow()),
                    _ => {
                        let mut xs = Vec::with_capacity(*arity);
                        for arg in self.env.iter() {
                            xs.push(format!("{}", arg.borrow()))
                        }
                        write!(f, "({})", xs.join(", "))
                    }
                }
            }
            Term::Record(ix) => {
                match ix.len() {
                    0 => write!(f, "@{{}}"),
                    n => {
                        let mut xs = Vec::with_capacity(n);
                        for (label, val) in ix.iter().zip(self.env.iter()) {
                            xs.push(format!("{} = {}", label, val.borrow()));
                        }
                        write!(f, "@{{ {} }}", xs.join(", "))
                    }
                }
            }
            other => {
                write!(f, "{}", other)
            }
        }
    }
}
