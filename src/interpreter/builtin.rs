use crate::runtime::builtin::Builtin;
use crate::syntax::ast::Lit;
use super::*;

pub fn builtin(f: Builtin, x: Value) -> Result<Value, Error> {
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
                return Err(Error::DivisionByZero);
            }
            Ok(Value::Lit(Lit::Int(a / b)))
        }
        Builtin::I64Mod => {
            let Value::Tuple(ts) = x else { panic!() };
            let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = ts.as_slice() else { panic!() };
            if *b == 0 {
                return Err(Error::DivisionByZero);
            }
            Ok(Value::Lit(Lit::Int(a % b)))
        }
    }
}
