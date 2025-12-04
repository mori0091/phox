use std::collections::HashMap;

use lalrpop_util::ParseError;

use crate::grammar::*;

use crate::error::Error;
use crate::syntax::ast::*;
use crate::syntax::lexer::*;
use crate::syntax::token::*;

use crate::module::*;
use crate::resolve::*;
use crate::typesys::*;
pub use crate::typesys::Pretty;

use crate::interpreter::*;

mod bootstrap;
use bootstrap::*;

pub mod loader;
use loader::*;

pub const DEFAULT_USER_ROOT_MODULE_NAME: &str = "__main__";

pub struct PhoxEngine {
    pub ctx: TypeContext,
    pub roots: RootModules,
    pub extern_symbol_envs: HashMap<Path, SymbolEnv>, // extern symbol table for each modules
    pub module_symbol_envs: HashMap<Path, SymbolEnv>, // local symbol table for each modules
    pub module_infer_ctxs: HashMap<Path, InferCtx>,   // kind_env, type_env, and trait_member_env for each modules
    pub module_value_envs: HashMap<Path, ValueEnv>,   // value_env for each modules
    pub impl_member_env: TraitMemberEnv, // implメンバの型スキーム集合 (ex. "f": { ∀ Int. Foo Int => Int -> Int, ∀ Bool. Foo Bool => Bool -> Bool })
    pub impl_env: ImplEnv,
}

impl PhoxEngine {
    pub fn new() -> Self {
        let mut phox = PhoxEngine {
            ctx: TypeContext::new(),
            roots: RootModules::new(),
            extern_symbol_envs: HashMap::new(),
            module_symbol_envs: HashMap::new(),
            module_infer_ctxs: HashMap::new(),
            module_value_envs: HashMap::new(),
            impl_member_env: TraitMemberEnv::new(),
            impl_env: ImplEnv::new(),
        };

        {
            let (_file, src) = load_module_src(&Path::absolute(vec!["core"])).unwrap();
            phox.new_core("core", &src);
        }
        {
            let (_file, src) = load_module_src(&Path::absolute(vec!["prelude"])).unwrap();
            phox.new_root("prelude", &src);
        }
        phox.new_root(DEFAULT_USER_ROOT_MODULE_NAME, "use ::prelude::*;");

        phox
    }

    // Bootstrap and register "::core" module.
    fn new_core(&mut self, name: &str, src: &str) {
        let root = Module::new_root(name);
        bootstrap(self, &root).expect("fatal error");
        self.roots.add(root.clone());
        self.eval_mod(&root, src).unwrap();
    }

    // Create and register new root module.
    fn new_root(&mut self, name: &str, src: &str) {
        let root = Module::new_root(name);
        self.roots.add(root.clone());
        self.eval_mod(&root, src).unwrap();
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
pub fn eval(src: &str) -> Result<(Value, TypeScheme), Error> {
    let mut phox = PhoxEngine::new();
    phox.eval(src)
}

impl PhoxEngine {
    /// Get "extern" SymbolEnv of the module.
    pub fn get_extern_symbol_env(&mut self, module: &RefModule) -> SymbolEnv {
        let path = module.borrow().path();
        self.extern_symbol_envs
            .entry(path)
            .or_insert_with(SymbolEnv::new)
            .clone()
    }
    /// Get top-level SymbolEnv of the module.
    pub fn get_symbol_env(&mut self, module: &RefModule) -> SymbolEnv {
        let path = module.borrow().path();
        self.module_symbol_envs
            .entry(path)
            .or_insert_with(SymbolEnv::new)
            .clone()
    }
    /// Get top-level InferCtx of the module.
    pub fn get_infer_ctx(&mut self, module: &RefModule) -> InferCtx {
        let path = module.borrow().path();
        self.module_infer_ctxs
            .entry(path)
            .or_insert_with(InferCtx::new)
            .clone()
    }
    /// Get top-level ValueEnv of the module.
    pub fn get_value_env(&mut self, module: &RefModule) -> ValueEnv {
        let path = module.borrow().path();
        self.module_value_envs
            .entry(path)
            .or_insert_with(ValueEnv::new)
            .clone()
    }

    /// Resolve an item.
    pub fn resolve_item(&mut self, module: &RefModule, item: &mut Item) -> Result<(), Error> {
        let symbol_env = &mut self.get_symbol_env(module);
        resolve_item(self, module, symbol_env, item)
    }

    /// Resolve and infer type scheme of an item.
    pub fn infer_item(&mut self, module: &RefModule, item: &mut Item) -> Result<TypeScheme, Error> {
        self.resolve_item(module, item)?;
        let icx = &mut self.get_infer_ctx(module);
        let ty = infer_item(self, module, icx, item)?;
        apply_trait_impls_item(self, module, item)?;
        Ok(generalize(&mut self.ctx, icx, &ty))
    }

    /// Resolve, infer type scheme, and evaluate an item.
    pub fn eval_item(&mut self, module: &RefModule, item: &mut Item) -> Result<(Value, TypeScheme), Error> {
        let sch = self
            .infer_item(module, item)
            .map_err(|e| Error::Message(format!("infer error: {e}")))?;
        let env = &mut self.get_value_env(module);
        let val = eval_item(self, module, env, &item)
            .map_err(|e| Error::Message(format!("eval error: {e}")))?;
        Ok((val, sch))
    }

    /// Resolve, infer type scheme, and evaluate items.
    pub fn eval_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<(Value, TypeScheme), Error> {
        let mut last = None;
        for mut item in items {
            let ret = self.eval_item(module, &mut item)?;
            last = Some(ret);
        }
        last.ok_or_else(|| Error::Message(format!("program contained no expression")))
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code.
    pub fn eval_mod(&mut self, module: &RefModule, src: &str) -> Result<(Value, TypeScheme), Error> {
        let mut items = parse(src)
            .map_err(|e| Error::Message(format!("parse error: {e:?}")))?;
        self.eval_items(module, &mut items)
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code in "::__main__" module.
    pub fn eval(&mut self, src: &str) -> Result<(Value, TypeScheme), Error> {
        let module = self.roots.get(DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
        self.eval_mod(&module, src)
    }
}

// -------------------------------------------------------------
/// Parse an expression. (for test)
pub fn parse_expr(src: &str) -> Result<Expr, Error> {
    let mut lexer = Lexer::new(src);
    ExprParser::new()
        .parse(&mut lexer)
        .map_err(|e| Error::Message(format!("parse error: {e:?}")))
}

// -------------------------------------------------------------
/// Infer type scheme of Expr AST. (for test)
pub fn infer_expr_scheme(ast: &mut Expr) -> Result<TypeScheme, Error> {
    let mut phox = PhoxEngine::new();
    let mut module = phox.roots.get(DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
    let mut item = Item::Expr(ast.clone());
    phox.infer_item(&mut module, &mut item)
        .map_err(|e| Error::Message(format!("infer error: {e}")))
}

/// Infer type of Expr AST. (for test)
pub fn infer_expr_type(ast: &mut Expr) -> Result<Type, Error> {
    let sch = infer_expr_scheme(ast)?;
    Ok(sch.target)
}

/// Parse and infer type scheme of an expression. (for test)
pub fn check_expr_scheme(src: &str) -> Result<TypeScheme, Error> {
    let mut ast = parse_expr(src)?;
    infer_expr_scheme(&mut ast)
}

/// Parse and infer type of an expression. (for test)
pub fn check_expr_type(src: &str) -> Result<Type, Error> {
    let sch = check_expr_scheme(src)?;
    Ok(sch.target)
}
