use std::collections::HashSet;
use std::rc::Rc;

use shared::SharedContext;
use viper::AstFactory;

use crate::utils::{
    EncodeOptions, MethodContext, ProgramToViper, ToViper, ToViperError, ToViperType,
    TranslationMode, TryToViper, TypeContext, ViperEncodeCtx,
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

        // add access permissions to arguments if structs
        let mut pres = self
            .args
            .iter()
            .filter_map(|a| a.precondition(true, ctx))
            .collect::<Vec<_>>();
        let pred_pres = ctx
            .model
            .predicates
            .clone()
            .into_iter()
            .map(|p| p.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        pres.extend(pred_pres);
        let mut posts = pres.clone();

        // Add postcondition (bounds of integers)
        posts.push(self.postcondition(ctx));

        let args_local_decls = self.args.to_viper(ctx);

        let body = self.body.to_viper(ctx)?;
        let body = ast.seqn(
            &[
                body,
                ast.label(ctx.return_label(), &[]),
                ast.refute(ast.false_lit(), ast.no_position()),
            ],
            &[],
        );

        ctx.set_mode(TranslationMode::PrePost);
        pres.extend(self.pres.to_viper(ctx)?);

        // add precondition about heap size: `requires alen(heap) == HEAP_SIZE`
        pres.insert(
            0,
            ast.eq_cmp(
                ctx.iarray.len_f(ctx.heap_var().1),
                ast.int_lit(ctx.options.heap_size as i64),
            ),
        );
        posts.extend(self.posts.to_viper(ctx)?);
        ctx.set_mode(TranslationMode::Normal);

        let mut base_args_local_decls = ctx.get_default_args().0;
        base_args_local_decls.extend(args_local_decls);

        Ok(ast.method(
            &self.fname,
            &base_args_local_decls,
            &[ast.local_var_decl(&self.retvar, ctx.get_type(&self.retvar)?.to_viper_type(ctx))],
            &pres,
            &posts,
            if self.trusted { None } else { Some(body) },
        ))
    }
}

fn extend_body<'a>(
    args: &[Arg],
    body: Option<viper::Expr<'a>>,
    ctx: &mut ViperEncodeCtx<'a>,
) -> Option<viper::Expr<'a>> {
    let ast = ctx.ast;
    let pres = args
        .iter()
        .filter_map(|a| a.precondition(true, ctx))
        .reduce(|acc, e| ast.and(acc, e));
    match (body, pres) {
        (Some(b), Some(p)) => Some(ast.and(p, b)),
        (Some(b), None) => Some(b),
        _ => None,
    }
}

impl<'a> TryToViper<'a> for Predicate {
    type Output = viper::Predicate<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let body = self.body.map(|e| e.to_viper(ctx)).transpose()?;
        let body = extend_body(&self.args, body, ctx);
        let args = self.args.to_viper(ctx);
        let mut base_args = ctx.get_default_args().0;
        base_args.extend(args);
        Ok(ast.predicate(&self.name, &base_args, body))
    }
}

impl<'a> TryToViper<'a> for Function {
    type Output = viper::Function<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        // set the type of `result` so it can be used in post-conditions
        ctx.typectx_get_mut()
            .set_type("result".into(), self.typ.clone());

        let mut pres = self
            .args
            .iter()
            .filter_map(|a| a.precondition(true, ctx))
            .collect::<Vec<_>>();
        pres.extend(self.pres.to_viper(ctx)?);
        let posts = self.posts.to_viper(ctx)?;
        let body = self.body.map(|b| b.to_viper(ctx)).transpose()?;

        let args = self.args.to_viper(ctx);
        let mut base_args = ctx.get_default_args().0;
        base_args.extend(args);

        Ok(ast.function(
            &self.name,
            &base_args,
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
        let mut pres = self
            .args
            .iter()
            .filter_map(|a| a.precondition(true, ctx))
            .collect::<Vec<_>>();
        pres.extend(self.pres.to_viper(ctx)?);
        let posts = self.posts.to_viper(ctx)?;

        let rettyps = self.rettyps.to_viper(ctx);
        let args = self.args.to_viper(ctx);
        let mut base_args = ctx.get_default_args().0;
        base_args.extend(args);

        Ok(ast.method(&self.name, &base_args, &rettyps, &pres, &posts, None))
    }
}

impl<'a> ProgramToViper<'a> for Program {
    fn to_viper(
        self,
        types: TypeContext,
        ast: AstFactory<'a>,
        options: EncodeOptions,
    ) -> Result<viper::Program<'a>, ToViperError> {
        // Create context for shared memory accesses
        let shared = Rc::new(SharedContext::new(&options, &self.shared));
        // Create method context for automatic unfolding/folding of function predicates
        let method_ctx = Rc::new(MethodContext::new(&self.functions));
        let model = self.model.clone();
        let fields = Rc::new(self.extern_fields);
        let extern_methods = self.extern_methods.clone();

        let mut predicate_names = self
            .predicates
            .iter()
            .map(|p| p.name.to_owned())
            .collect::<HashSet<_>>();

        let predicates = self
            .predicates
            .into_iter()
            .map(|p| {
                let mut ctx = ViperEncodeCtx::new(
                    types.clone(),
                    predicate_names.clone(),
                    ast,
                    options,
                    shared.clone(),
                    method_ctx.clone(),
                    model.clone(),
                    extern_methods.clone(),
                );
                ctx.set_mode(TranslationMode::PrePost);
                p.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        // add abstract predicates to predicate names set
        for pred in self.extern_predicates {
            predicate_names.insert(pred);
        }
        for pred in &self.model.predicates {
            if let Expr::FunctionCall(call) = pred {
                predicate_names.insert(call.fname.trim_start_matches("f_").to_owned());
            }
        }

        let mut functions = self
            .viper_functions
            .into_iter()
            .map(|f| {
                let mut ctx = ViperEncodeCtx::new(
                    types.clone(),
                    predicate_names.clone(),
                    ast,
                    options,
                    shared.clone(),
                    method_ctx.clone(),
                    model.clone(),
                    extern_methods.clone(),
                );
                ctx.set_mode(TranslationMode::PrePost);
                f.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let abstract_methods = self
            .methods
            .into_iter()
            .map(|m| {
                let mut ctx = ViperEncodeCtx::new(
                    types.clone(),
                    predicate_names.clone(),
                    ast,
                    options,
                    shared.clone(),
                    method_ctx.clone(),
                    model.clone(),
                    extern_methods.clone(),
                );
                ctx.set_mode(TranslationMode::PrePost);
                m.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let program_methods = self
            .functions
            .into_iter()
            .map(|f| {
                let mut ctx = ViperEncodeCtx::new(
                    types.clone(),
                    predicate_names.clone(),
                    ast,
                    options,
                    shared.clone(),
                    method_ctx.clone(),
                    model.clone(),
                    extern_methods.clone(),
                );
                f.to_viper(&mut ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let (domains, fields, mut methods, fs) = create_viper_prelude(ast, self.model, options);
        methods.extend(abstract_methods.iter());
        methods.extend(program_methods.iter());
        functions.extend(fs.iter());
        Ok(ast.program(&domains, &fields, &functions, &predicates, &methods))
    }
}
