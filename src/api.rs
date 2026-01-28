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
    pub ctx: UnifiedContext,
    pub roots: RootModules,
    pub extern_symbol_envs: HashMap<Path, SymbolEnv>, // extern symbol table for each modules
    pub module_symbol_envs: HashMap<Path, SymbolEnv>, // local symbol table for each modules
    pub module_infer_ctxs: HashMap<Path, InferCtx>,   // kind_env, type_env, and trait_member_env for each modules
    pub module_value_envs: HashMap<Path, ValueEnv>,   // value_env for each modules
    pub starlet_env: StarletEnv,                      // starlet_env ; the set of `SchemeTeamplate<TypedStarlet>`s for each defined `*let`.
    pub impl_env: ImplEnv,                            // impl_env ; the set of `SchemeTeamplate<TypedImpl>`s for each defined `impl`.
}

impl PhoxEngine {
    pub fn new() -> Self {
        let mut phox = PhoxEngine {
            ctx: UnifiedContext::new(),
            roots: RootModules::new(),
            extern_symbol_envs: HashMap::new(),
            module_symbol_envs: HashMap::new(),
            module_infer_ctxs: HashMap::new(),
            module_value_envs: HashMap::new(),
            starlet_env: StarletEnv::new(),
            impl_env: ImplEnv::new(),
        };

        match phox.boot() {
            Ok(_) => {}
            Err(e) => {
                eprintln!("error occurred during starting up Phox...");
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }

        phox
    }

    fn boot(&mut self) -> Result<(), Error> {
        let (_file, src) = load_module_src(&Path::absolute(vec!["core"]))?;
        self.new_core("core", &src)?;

        let (_file, src) = load_module_src(&Path::absolute(vec!["prelude"]))?;
        self.new_root("prelude", &src)?;

        self.new_root(DEFAULT_USER_ROOT_MODULE_NAME, "use ::prelude::*;")?;

        Ok(())
    }

    // Bootstrap and register "::core" module.
    fn new_core(&mut self, name: &str, src: &str) -> Result<(), Error> {
        let root = Module::new_root(name);
        bootstrap(self, &root).expect("fatal error");
        self.roots.add(root.clone());
        self.run_mod(&root, src)?;
        Ok(())
    }

    // Create and register new root module.
    fn new_root(&mut self, name: &str, src: &str) -> Result<(), Error> {
        let root = Module::new_root(name);
        self.roots.add(root.clone());
        self.run_mod(&root, src)?;
        Ok(())
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
    phox.run(src)
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
}

impl PhoxEngine {
    /// Resolve an item.
    pub fn resolve_item(&mut self, module: &RefModule, item: &mut Item) -> Result<(), Error> {
        let symbol_env = &mut self.get_symbol_env(module);
        let param_map = &mut HashMap::new();
        resolve_item(self, module, symbol_env, param_map, item)
    }

    /// Infer type scheme of an item.
    pub fn infer_item(&mut self, module: &RefModule, item: &mut Item) -> Result<Type, Error> {
        let icx = &mut self.get_infer_ctx(module);
        let (ty, cs) = infer_item(self, module, icx, item)?;
        item.constraints = cs;
        Ok(ty)
    }

    pub fn apply_item(&mut self, module: &RefModule, item: &mut Item) -> Result<(), Error> {
        apply_trait_impls_item(self, module, item)
    }

    /// Evaluate an item.
    pub fn eval_item(&mut self, module: &RefModule, item: &mut Item) -> Result<Value, Error> {
        let env = &mut self.get_value_env(module);
        let val = eval_item(self, module, env, &item)
            .map_err(|e| Error::Message(format!("eval error: {e}")))?;
        Ok(val)
    }

    // ---------------------------------------------------------

    /// Infer type scheme for each items.
    pub fn infer_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<Type, Error> {
        let mut last = None;
        for item in items {
            self.resolve_item(module, item)?;

            let ty = self
                .infer_item(module, item)
                .map_err(|e| Error::Message(format!("infer error: {e}")))?;
            last = Some(ty);

            register_item(self, module, item)?;
        }
        last.ok_or_else(|| Error::Message(format!("program contained no item")))
    }

    pub fn solve_items(&mut self, _module: &RefModule, items: &mut Vec<Item>) -> Result<(), Error> {
        for item in items {
            solve_item(self, item)?;
        }
        Ok(())
    }

    pub fn apply_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<(), Error> {
        for item in items {
            self.apply_item(module, item)?;
        }
        Ok(())
    }

    /// Evaluate for each items.
    pub fn eval_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<Value, Error> {
        let mut last = None;
        for item in items {
            let ret = self.eval_item(module, item)?;
            last = Some(ret);
        }
        last.ok_or_else(|| Error::Message(format!("program contained no expression")))
    }

    /// Resolve and infer type scheme for each items, then evaluate them.
    pub fn run_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<(Value, TypeScheme), Error> {
        // let mod_path = module.borrow().path().pretty();
        // eprintln!("mod {mod_path}", );

        // eprintln!("  {mod_path} resolve, infer, register...");
        let ty = self.infer_items(module, items)?;
        // eprintln!("  ty: {}", ty.repr(&mut self.ctx));

        // eprintln!("  {mod_path} solve...");
        self.solve_items(module, items)?;
        // eprintln!("  ty: {}", ty.repr(&mut self.ctx));

        // eprintln!("  {mod_path} apply...");
        self.apply_items(module, items)?;

        // eprintln!("  {mod_path} eval...");
        let val = self.eval_items(module, items)?;

        // eprintln!("  {mod_path} done");
        // eprintln!();

        let icx = &mut self.get_infer_ctx(module);
        let sch = generalize(&mut self.ctx, icx, &ty);
        Ok((val, sch))
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code.
    pub fn run_mod(&mut self, module: &RefModule, src: &str) -> Result<(Value, TypeScheme), Error> {
        let mut items = parse(src)
            .map_err(|e| Error::Message(format!("parse error: {e:?}")))?;
        self.run_items(module, &mut items)
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code in "::__main__" module.
    pub fn run(&mut self, src: &str) -> Result<(Value, TypeScheme), Error> {
        let module = self.roots.get(DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
        self.run_mod(&module, src)
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
    let item = Item::expr(ast.clone());
    let (_val, sch) = phox.run_items(&mut module, &mut vec![item])?;
    Ok(sch)
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
