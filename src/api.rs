use lalrpop_util::ParseError;
use crate::grammar::{ItemListParser, ExprParser};

use crate::syntax::ast::{Program, Item, Expr};
use crate::syntax::lexer::Lexer;
use crate::syntax::token::{Token, LexicalError};

use crate::resolve::resolve_item;

use crate::typesys::{Type, TypeScheme};
use crate::typesys::TypeContext;
use crate::typesys::{infer_item, apply_trait_impls_item, generalize};

use crate::interpreter::Value;
use crate::interpreter::eval_item;
use crate::module::{Module, RefModule, RootModules};

use crate::prelude::*;

pub const DEFAULT_USER_ROOT_MODULE_NAME: &str = "__main__";

pub struct PhoxEngine {
    pub ctx: TypeContext,
    pub roots: RootModules,
}

impl PhoxEngine {
    pub fn new() -> Self {
        let mut phox = PhoxEngine {
            ctx: TypeContext::new(),
            roots: RootModules::new(),
        };
        let mut prelude = Module::new_root(&mut phox.ctx);
        phox.eval_mod(&mut prelude, PRELUDE).unwrap();
        phox.roots.add("prelude".to_string(), prelude);

        // *** temporal implementation ***
        // currently user-root-module is just a clone of `prelude`.
        let usermod = phox.roots.get("prelude").unwrap();
        phox.roots.add(DEFAULT_USER_ROOT_MODULE_NAME.to_string(), usermod.clone());

        phox
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
    let mut phox = PhoxEngine::new();
    phox.eval(src)
}

impl PhoxEngine {
    /// Resolve an item.
    fn resolve_item(&mut self, module: &mut RefModule, item: &mut Item) -> Result<(), String> {
        resolve_item(self, &mut module.borrow_mut(), item)
            .map_err(|e| format!("resolve error: {e}"))
    }

    /// Resolve and infer type scheme of an item.
    fn infer_item(&mut self, module: &mut RefModule, item: &mut Item) -> Result<TypeScheme, String> {
        self.resolve_item(module, item)?;
        let ty = infer_item(self, &mut module.borrow_mut().icx, item)
            .map_err(|e| format!("infer error: {e}"))?;

        apply_trait_impls_item(item, &mut self.ctx, &mut module.borrow_mut())
            .map_err(|e| format!("infer error: {e}"))?;

        Ok(generalize(&mut self.ctx, &module.borrow_mut().icx, &ty))
    }

    /// Resolve, infer type scheme, and evaluate an item.
    pub fn eval_item(&mut self, module: &mut RefModule, item: &mut Item) -> Result<(Value, TypeScheme), String> {
        let sch = self.infer_item(module, item)?;
        let val = eval_item(&item, &mut module.borrow_mut().env);
        Ok((val, sch))
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code.
    pub fn eval_mod(&mut self, module: &mut RefModule, src: &str) -> Result<(Value, TypeScheme), String> {
        let tops = parse(src).map_err(|e| format!("parse error: {e:?}"))?;
        let mut last = None;
        for mut item in tops {
            let res = self.eval_item(module, &mut item)?;
            last = Some(res);
        }
        last.ok_or_else(|| "program contained no expression".to_string())
    }

    pub fn eval(&mut self, src: &str) -> Result<(Value, TypeScheme), String> {
        let mut module = self.roots.get(DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
        self.eval_mod(&mut module, src)
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
    let mut phox = PhoxEngine::new();
    let mut module = phox.roots.get("prelude").unwrap();
    phox.infer_item(&mut module, &mut Item::Expr(ast.clone()))
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
