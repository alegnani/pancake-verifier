use crate::{
    pancake::{self, Shape},
    viper_prelude::create_viper_prelude,
};

use super::context::ViperEncodeCtx;

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
            .local_var_decl(&ctx.mangle_arg(&self.name), self.shape.to_viper_type(ctx))
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
        let copied_args = self
            .args
            .iter()
            .map(|a| {
                ast.local_var_decl(&ctx.mangle_var(&a.name), a.shape.to_viper_type(ctx))
                    .into()
            })
            .collect::<Vec<_>>();

        for arg in &self.args {
            ctx.type_map
                .insert(ctx.mangle_var(&arg.name), arg.shape.clone());
            ctx.type_map.insert(arg.name.clone(), arg.shape.clone());
        }

        let mut args_assigns = self
            .args
            .iter()
            .map(|a| {
                let typ = a.shape.to_viper_type(ctx);
                ast.local_var_assign(
                    ast.local_var(&ctx.mangle_var(&a.name), typ),
                    ast.local_var(&ctx.mangle_arg(&a.name), typ),
                )
            })
            .collect::<Vec<_>>();

        let body = self.body.to_viper(ctx);

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
