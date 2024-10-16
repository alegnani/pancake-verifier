use std::collections::HashSet;

use viper::AstFactory;

use crate::utils::{
    EncodeOptions, ProgramToViper, ToViper, ToViperError, ToViperType, TranslationMode, TryToViper,
    TypeContext, ViperEncodeCtx,
};
use crate::viper_prelude::create_viper_prelude;

use crate::ir::*;

impl<'a> ToViper<'a> for Arg {
    type Output = viper::LocalVarDecl<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        ctx.ast
            .local_var_decl(&self.name, self.typ.to_viper_type(ctx))
    }
}

impl<'a> TryToViper<'a> for FnDec {
    type Output = viper::Method<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        let mut pres = self
            .args
            .iter()
            .filter_map(|a| a.permission(ctx))
            .collect::<Vec<_>>();

        // Copy all the parameters as they are read-only in Viper
        let mut args_local_decls = self.args.to_viper(ctx);

        let body = self.body.to_viper(ctx)?;
        let body = ast.seqn(
            &[
                body,
                ast.label(ctx.return_label(), &[]),
                ast.refute(ast.false_lit(), ast.no_position()),
            ],
            &[],
        );

        pres.extend(&ctx.pres);
        // add precondition about heap size: `requires alen(heap) == HEAP_SIZE`
        pres.insert(
            0,
            ast.eq_cmp(
                ctx.iarray.len_f(ctx.heap_var().1),
                ast.int_lit(ctx.options.heap_size as i64),
            ),
        );

        args_local_decls.insert(0, ctx.heap_var().0);

        Ok(ast.method(
            &self.fname,
            &args_local_decls,
            &[ast.local_var_decl(&self.retvar, ctx.get_type(&self.retvar)?.to_viper_type(ctx))],
            &pres,
            &ctx.posts,
            Some(body),
        ))
    }
}

impl<'a> TryToViper<'a> for Predicate {
    type Output = viper::Predicate<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let mut args = self.args.to_viper(ctx);
        let body = self.body.map(|e| e.to_viper(ctx)).transpose()?;
        args.insert(0, ctx.heap_var().0);
        Ok(ast.predicate(&self.name, &args, body))
    }
}

fn partition_annotation_types(
    annotations: Vec<Annotation>,
) -> (Vec<Annotation>, Vec<Annotation>, Vec<Annotation>) {
    let (pres, others): (Vec<_>, Vec<_>) = annotations
        .into_iter()
        .partition(|e| matches!(e.typ, AnnotationType::Precondition));
    let (posts, others): (Vec<_>, Vec<_>) = others
        .into_iter()
        .partition(|e| matches!(e.typ, AnnotationType::Postcondition));
    (pres, posts, others)
}

impl<'a> TryToViper<'a> for Function {
    type Output = viper::Function<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let mut args = self.args.to_viper(ctx);

        let (pres, posts, others) = partition_annotation_types(self.preposts);

        if !others.is_empty() {
            return Err(ToViperError::InvalidAnnotation(others));
        }

        let pres = pres
            .into_iter()
            .map(|e| e.expr.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let posts = posts
            .into_iter()
            .map(|e| e.expr.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let body = self.body.map(|b| b.to_viper(ctx)).transpose()?;

        args.insert(0, ctx.heap_var().0);
        Ok(ast.function(
            &self.name,
            &args,
            self.typ.to_viper_type(ctx),
            &pres,
            &posts,
            ast.no_position(),
            body,
        ))
    }
}

impl<'a> TryToViper<'a> for AbstractMethod {
    type Output = viper::Method<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let mut args = self.args.to_viper(ctx);
        let rettyps = self.rettyps.to_viper(ctx);

        let (pres, posts, others) = partition_annotation_types(self.preposts);

        if !others.is_empty() {
            return Err(ToViperError::InvalidAnnotation(others));
        }

        let pres = pres
            .into_iter()
            .map(|e| e.expr.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let posts = posts
            .into_iter()
            .map(|e| e.expr.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        args.insert(0, ctx.heap_var().0);
        Ok(ast.method(&self.name, &args, &rettyps, &pres, &posts, None))
    }
}

impl<'a> ProgramToViper<'a> for Program {
    fn to_viper(
        self,
        types: TypeContext,
        ast: AstFactory<'a>,
        options: EncodeOptions,
    ) -> Result<viper::Program<'a>, ToViperError> {
        let (predicates, predicate_names): (Vec<_>, HashSet<_>) = self
            .predicates
            .into_iter()
            .map(|p| {
                let pred_name = p.name.clone();
                let mut ctx = ViperEncodeCtx::new(types.clone(), HashSet::new(), ast, options);
                ctx.set_mode(TranslationMode::PrePost);

                (p.to_viper(&mut ctx), pred_name)
            })
            .unzip();
        let predicates = predicates.into_iter().collect::<Result<Vec<_>, _>>()?;

        let mut functions = self
            .viper_functions
            .into_iter()
            .map(|f| {
                let mut ctx =
                    ViperEncodeCtx::new(types.clone(), predicate_names.clone(), ast, options);
                ctx.set_mode(TranslationMode::PrePost);
                f.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let abstract_methods = self
            .methods
            .into_iter()
            .map(|m| {
                let mut ctx =
                    ViperEncodeCtx::new(types.clone(), predicate_names.clone(), ast, options);
                ctx.set_mode(TranslationMode::PrePost);
                m.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let program_methods = self
            .functions
            .into_iter()
            .map(|f| {
                let mut ctx =
                    ViperEncodeCtx::new(types.clone(), predicate_names.clone(), ast, options);
                f.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let (domains, fields, mut methods, fs) = create_viper_prelude(ast);
        methods.extend(abstract_methods.iter());
        methods.extend(program_methods.iter());
        functions.extend(fs.iter());
        Ok(ast.program(&domains, &fields, &functions, &predicates, &methods))
    }
}
