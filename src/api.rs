use lalrpop_util::ParseError;
use crate::grammar::{ItemListParser, ExprParser};

use crate::syntax::ast::{Program, Item, Expr};
use crate::syntax::lexer::Lexer;
use crate::syntax::token::{Token, LexicalError};

use crate::resolve::resolve_item;

use crate::typesys::{Type, TypeScheme};
use crate::typesys::{TypeContext, InferCtx, ImplEnv};
use crate::typesys::{infer_item, apply_trait_impls_item, generalize};

use crate::interpreter::{Value, Env};
use crate::interpreter::{initial_env, eval_item};

use crate::prelude::*;

pub struct Bootstrap {
    pub ctx: TypeContext,
    pub icx: InferCtx,
    pub impl_env: ImplEnv,
    pub env: Env,
}

impl Bootstrap {
    pub fn new() -> Self {
        let mut ctx = TypeContext::new();
        let icx = InferCtx::initial(&mut ctx);
        let impl_env = ImplEnv::new();
        let env = initial_env();
        let mut boot = Bootstrap {
            ctx,
            icx,
            impl_env,
            env,
        };
        boot.eval(PRELUDE).unwrap();
        boot
    }
}

// -------------------------------------------------------------
/// Parse list of items.
pub fn parse_items(src: &str) -> Result<Vec<Item>, ParseError<usize, Token, LexicalError>> {
    let mut lexer = Lexer::new(src);
    ItemListParser::new().parse(&mut lexer)
}

/// Parse a program.
pub fn parse(src: &str) -> Result<Program, ParseError<usize, Token, LexicalError>> {
    parse_items(src)
}

/// Parse, infer type scheme, and evaluate of a program.
pub fn eval(src: &str) -> Result<(Value, TypeScheme), String> {
    Bootstrap::new().eval(src)
}

impl Bootstrap {
    /// Resolve an item.
    fn resolve_item(&mut self, item: &mut Item) -> Result<(), String> {
        resolve_item(&mut self.ctx, &mut self.icx, &mut self.impl_env, &mut self.env, item)
            .map_err(|e| format!("resolve error: {e}"))
    }

    /// Resolve and infer type scheme of an item.
    fn infer_item(&mut self, item: &mut Item) -> Result<TypeScheme, String> {
        self.resolve_item(item)?;
        let ty = infer_item(&mut self.ctx, &mut self.icx, item)
            .map_err(|e| format!("infer error: {e}"))?;

        apply_trait_impls_item(item, &mut self.ctx, &self.icx, &self.impl_env)
            .map_err(|e| format!("infer error: {e}"))?;

        Ok(generalize(&mut self.ctx, &self.icx, &ty))
    }

    /// Resolve, infer type scheme, and evaluate an item.
    pub fn eval_item(&mut self, item: &mut Item) -> Result<(Value, TypeScheme), String> {
        let sch = self.infer_item(item)?;
        let val = eval_item(&item, &mut self.env);
        Ok((val, sch))
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code.
    pub fn eval(&mut self, src: &str) -> Result<(Value, TypeScheme), String> {
        let tops = parse(src).map_err(|e| format!("parse error: {e:?}"))?;
        let mut last = None;
        for mut item in tops {
            let res = self.eval_item(&mut item)?;
            last = Some(res);
        }
        last.ok_or_else(|| "program contained no expression".to_string())
    }
}

// -------------------------------------------------------------
/// Parse an expression. (for test)
pub fn parse_expr(src: &str) -> Result<Expr, String> {
    let mut lexer = Lexer::new(src);
    ExprParser::new()
        .parse(&mut lexer)
        .map_err(|e| format!("parse error: {e:?}"))
}

// -------------------------------------------------------------
/// Infer type scheme of Expr AST. (for test)
pub fn infer_expr_scheme(ast: &mut Expr) -> Result<TypeScheme, String> {
    let mut boot = Bootstrap::new();
    boot.infer_item(&mut Item::Expr(ast.clone()))
}

/// Infer type of Expr AST. (for test)
pub fn infer_expr_type(ast: &mut Expr) -> Result<Type, String> {
    let sch = infer_expr_scheme(ast)?;
    Ok(sch.target)
}

/// Parse and infer type scheme of an expression. (for test)
pub fn check_expr_scheme(src: &str) -> Result<TypeScheme, String> {
    let mut ast = parse_expr(src)?;
    infer_expr_scheme(&mut ast)
}

/// Parse and infer type of an expression. (for test)
pub fn check_expr_type(src: &str) -> Result<Type, String> {
    let sch = check_expr_scheme(src)?;
    Ok(sch.target)
}
