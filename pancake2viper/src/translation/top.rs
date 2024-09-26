use viper::{AstFactory, Declaration};

use crate::{
    pancake::{self, Shape},
    utils::ViperUtils,
    viper_prelude::create_viper_prelude,
};

use super::{
    context::{EncodeOptions, ViperEncodeCtx},
    mangler::Mangler,
};

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
        let mangled_arg = ctx.mangler.new_arg(self.name.clone());
        ctx.set_type(mangled_arg.clone(), self.shape.clone());
        ctx.ast
            .local_var_decl(&mangled_arg, self.shape.to_viper_type(ctx))
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

        // Copy all the parameters as they are read-only in Viper
        let mut args_local_decls = self
            .args
            .iter()
            .map(|a| a.clone().to_viper(ctx))
            .collect::<Vec<_>>();

        let (args_decls, mut args_assigns): (Vec<Declaration>, Vec<_>) = self
            .args
            .iter()
            .map(|a| {
                let typ = a.shape.to_viper_type(ctx);
                let mangled_lhs = ctx.mangler.new_scoped_var(a.name.clone());
                ctx.set_type(mangled_lhs.clone(), a.shape.clone());
                let lhs = ast.new_var(&mangled_lhs, typ);
                let decl: Declaration = lhs.0.into();
                (
                    decl,
                    ctx.ast
                        .local_var_assign(lhs.1, ast.local_var(&Mangler::mangle_arg(&a.name), typ)),
                )
            })
            .unzip();

        let body = self.body.to_viper(ctx);

        args_local_decls.insert(0, ctx.heap_var().0);

        args_assigns.extend_from_slice(&[body, ast.label(ctx.return_label(), &[])]);
        let body = ast.seqn(&args_assigns, &args_decls);

        ast.method(
            &ctx.mangler.mangle_fn(&self.fname),
            &args_local_decls,
            &[ctx.return_var().0],
            &ctx.pres,
            &ctx.posts,
            Some(body),
        )
    }
}

pub trait ProgramToViper<'a> {
    fn to_viper(self, ast: AstFactory<'a>, options: EncodeOptions) -> viper::Program<'a>;
}

impl<'a> ProgramToViper<'a> for pancake::Program {
    fn to_viper(self, ast: AstFactory<'a>, options: EncodeOptions) -> viper::Program<'a> {
        let program_methods = self
            .functions
            .into_iter()
            .map(|f| {
                let mut ctx = ViperEncodeCtx::new(f.fname.clone(), ast, options);
                f.to_viper(&mut ctx)
            })
            .collect::<Vec<_>>();
        let (domains, fields, mut methods) = create_viper_prelude(ast);
        methods.extend(program_methods.iter());
        ast.program(&domains, &fields, &[], &[], &methods)
    }
}
