use viper::{AstFactory, Declaration};

use crate::{
    utils::ViperUtils, viper_prelude::create_viper_prelude, ProgramToViper, ToViper, ToViperType,
};

use crate::ir::*;

use super::{
    utils::{EncodeOptions, Mangler},
    ViperEncodeCtx,
};

impl<'a> ToViper<'a> for Arg {
    type Output = viper::LocalVarDecl<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let mangled_arg = ctx.mangler.new_arg(self.name.clone());
        ctx.set_type(mangled_arg.clone(), self.shape.clone());
        ctx.ast
            .local_var_decl(&mangled_arg, self.shape.to_viper_type(ctx))
    }
}

impl<'a> ToViper<'a> for FnDec {
    type Output = viper::Method<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
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

impl<'a> ProgramToViper<'a> for Program {
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
