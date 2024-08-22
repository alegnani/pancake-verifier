use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::pancake;

use super::viper_prelude::create_viper_prelude;

pub struct ViperEncodeCtx<'a> {
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    fresh_counter: u64,
    while_counter: u64,
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self {
            ast,
            stack: vec![],
            declarations: vec![],
            fresh_counter: 0,
            while_counter: 0,
        }
    }

    pub fn child(&self) -> Self {
        Self {
            ast: self.ast,
            stack: vec![],
            declarations: vec![],
            fresh_counter: self.fresh_counter,
            while_counter: self.while_counter,
        }
    }

    pub fn fresh_var(&mut self) -> String {
        let fresh = format!("f_{}", self.fresh_counter);
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

    pub fn return_decl(&self) -> viper::LocalVarDecl {
        self.ast
            .local_var_decl(self.return_var_name(), self.ast.int_type())
    }

    pub fn return_var(&self) -> viper::Expr {
        self.ast
            .local_var(self.return_var_name(), self.ast.int_type())
    }

    pub fn new_while_ctx(&mut self) {
        self.while_counter += 1;
    }

    pub fn mangle_var(&self, var: &str) -> String {
        format!("_{}", var)
    }

    pub fn pop_decls(&mut self) -> Vec<Declaration<'a>> {
        self.declarations
            .drain(..)
            .map(LocalVarDecl::into)
            .collect()
    }

    pub fn heap_var(&self) -> viper::Expr {
        self.ast.local_var("heap", self.ast.ref_type())
    }
}

pub trait ToViper<'a, T> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> T;
}

impl<'a> ToViper<'a, viper::LocalVarDecl<'a>> for pancake::Arg {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::LocalVarDecl<'a> {
        let ast = ctx.ast;
        ast.local_var_decl(&ctx.mangle_var(&self.name), ast.int_type())
    }
}

impl<'a> ToViper<'a, viper::Method<'a>> for pancake::FnDec {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Method<'a> {
        let ast = ctx.ast;
        // TODO: mangle arg names differently ("f_{}"), declare and assign to
        // local variables as Viper args are immutable
        let body = ast.seqn(
            &[self.body.to_viper(ctx), ast.label(ctx.return_label(), &[])],
            &[],
        );
        let args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        ast.method(
            &self.fname,
            &args,
            &[ctx.return_decl()],
            &[],
            &[],
            Some(body),
        )
    }
}

impl<'a> ToViper<'a, viper::Program<'a>> for pancake::Program {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Program<'a> {
        let ast = ctx.ast;
        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_viper(ctx))
            .collect::<Vec<_>>();
        let (domains, fields) = create_viper_prelude(ast);
        ast.program(&domains, &fields, &[], &[], &functions)
    }
}
