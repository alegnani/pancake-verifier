use std::collections::HashMap;

use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::{pancake::Shape, viper_prelude::IArrayHelper};

pub struct ViperEncodeCtx<'a> {
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    pub type_map: HashMap<String, Shape>,
    fresh_counter: u64,
    while_counter: u64,
    pub iarray: IArrayHelper<'a>,
    pub options: EncodeOptions,
}

#[derive(Clone, Copy)]
pub struct EncodeOptions {
    pub expr_unrolling: bool,
    pub assert_aligned_accesses: bool,
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            expr_unrolling: false,
            assert_aligned_accesses: true,
        }
    }
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(ast: AstFactory<'a>, options: EncodeOptions) -> Self {
        Self {
            ast,
            stack: vec![],
            declarations: vec![],
            type_map: HashMap::new(),
            fresh_counter: 0,
            while_counter: 0,
            iarray: IArrayHelper::new(ast),
            options,
        }
    }

    pub fn child(&self) -> Self {
        Self {
            ast: self.ast,
            stack: vec![],
            declarations: vec![],
            type_map: self.type_map.clone(),
            fresh_counter: self.fresh_counter,
            while_counter: self.while_counter,
            iarray: self.iarray,
            options: self.options,
        }
    }

    pub fn fresh_var(&mut self) -> String {
        let fresh = format!("_fr{}", self.fresh_counter);
        self.fresh_counter += 1;
        fresh
    }

    pub fn current_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter)
    }

    pub fn current_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter)
    }

    pub fn return_label(&self) -> &'static str {
        "return_label"
    }

    fn return_var_name(&self) -> &'static str {
        "retval"
    }

    pub fn return_var(&self) -> (viper::LocalVarDecl, viper::Expr) {
        (
            self.ast
                .local_var_decl(self.return_var_name(), self.ast.int_type()),
            self.ast
                .local_var(self.return_var_name(), self.ast.int_type()),
        )
    }

    pub fn new_while_ctx(&mut self) {
        self.while_counter += 1;
    }

    pub fn mangle_var(&self, var: &str) -> String {
        format!("_{}", var)
    }

    pub fn mangle_fn(&self, fname: &str) -> String {
        format!("f_{}", fname)
    }

    pub fn mangle_arg(&self, arg: &str) -> String {
        format!("arg_{}", arg)
    }

    pub fn pop_decls(&mut self) -> Vec<Declaration<'a>> {
        self.declarations
            .drain(..)
            .map(LocalVarDecl::into)
            .collect()
    }

    pub fn heap_type(&self) -> viper::Type {
        self.iarray.get_type()
    }

    pub fn heap_var(&self) -> (viper::LocalVarDecl, viper::Expr) {
        (
            self.ast.local_var_decl("heap", self.heap_type()),
            self.ast.local_var("heap", self.heap_type()),
        )
    }
}
