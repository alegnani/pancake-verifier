use std::collections::HashMap;

use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::{
    pancake::{self, Shape},
    viper_prelude::{create_viper_prelude, IArrayHelper},
};

pub struct ViperEncodeCtx<'a> {
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    pub type_map: HashMap<String, Shape>,
    fresh_counter: u64,
    while_counter: u64,
    pub iarray: IArrayHelper<'a>,
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self {
            ast,
            stack: vec![],
            declarations: vec![],
            type_map: HashMap::new(),
            fresh_counter: 0,
            while_counter: 0,
            iarray: IArrayHelper::new(ast),
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

pub trait ToViper<'a, T> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> T;
    fn to_viper_with_pos(&self, ctx: &mut ViperEncodeCtx<'a>, pos: viper::Position) -> T {
        todo!()
    }
}
pub trait ToViperType<'a> {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a>;
}

pub trait ToShape<'a> {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape;
}

impl<'a> ToViper<'a, viper::LocalVarDecl<'a>> for pancake::Arg {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::LocalVarDecl<'a> {
        ctx.ast
            .local_var_decl(&self.name, self.shape.to_viper_type(ctx))
    }
}

impl<'a> ToShape<'a> for pancake::Arg {
    fn shape(&self, _ctx: &ViperEncodeCtx<'a>) -> Shape {
        self.shape.clone()
    }
}

impl<'a> ToViper<'a, viper::Method<'a>> for pancake::FnDec {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Method<'a> {
        let ast = ctx.ast;

        let body = self.body.to_viper(ctx);

        // Copy all the parameters as they are read-only in Viper
        let copied_args = self
            .args
            .iter()
            .map(|a| {
                ast.local_var_decl(&ctx.mangle_var(&a.name), a.shape.to_viper_type(ctx))
                    .into()
            })
            .collect::<Vec<_>>();

        let mut args_assigns = self
            .args
            .iter()
            .map(|a| {
                let typ = a.shape.to_viper_type(ctx);
                ast.local_var_assign(
                    ast.local_var(&ctx.mangle_var(&a.name), typ),
                    ast.local_var(&a.name, typ),
                )
            })
            .collect::<Vec<_>>();

        let mut args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        args.insert(0, ctx.heap_var().0);

        args_assigns.extend_from_slice(&[body, ast.label(ctx.return_label(), &[])]);
        let body = ast.seqn(&args_assigns, &copied_args);

        ast.method(
            &ctx.mangle_fn(&self.fname),
            &args,
            &[ctx.return_var().0],
            &[],
            &[],
            Some(body),
        )
    }
}

impl<'a> ToViper<'a, viper::Program<'a>> for pancake::Program {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Program<'a> {
        let ast = ctx.ast;
        let program_methods = self
            .functions
            .into_iter()
            .map(|f| f.to_viper(ctx))
            .collect::<Vec<_>>();
        let (domains, fields, mut methods) = create_viper_prelude(ast);
        methods.extend(program_methods.iter());
        ast.program(&domains, &fields, &[], &[], &methods)
    }
}
