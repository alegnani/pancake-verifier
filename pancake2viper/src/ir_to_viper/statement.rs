use crate::ir;
use crate::utils::{
    ToViperError, ToViperType, TranslationMode, TryToShape, TryToViper, ViperEncodeCtx, ViperUtils,
};

impl<'a> TryToViper<'a> for ir::Stmt {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        use ir::Stmt::*;
        let stmt = match self {
            Skip => ast.comment("skip"),
            Break => ast.goto(&ctx.outer_break_label()),
            Continue => ast.goto(&ctx.outer_continue_label()),
            x => match x {
                Annotation(annot) => annot.to_viper(ctx),
                Definition(def) => def.to_viper(ctx),
                Assign(ass) => ass.to_viper(ctx),
                Return(ret) => ret.to_viper(ctx),
                If(ifs) => ifs.to_viper(ctx),
                While(whiles) => whiles.to_viper(ctx),
                Seq(seq) => seq.to_viper(ctx),
                Call(call) => call.to_viper(ctx),
                ExtCall(ext) => ext.to_viper(ctx),
                Store(store) => store.to_viper(ctx),
                StoreBits(store) => store.to_viper(ctx),
                SharedStore(store) => store.to_viper(ctx),
                SharedStoreBits(store) => store.to_viper(ctx),
                SharedLoad(load) => load.to_viper(ctx),
                SharedLoadBits(load) => load.to_viper(ctx),
                _ => unreachable!(),
            }?,
        };
        ctx.stack.push(stmt);

        let decls = ctx
            .declarations
            .drain(..)
            .map(|d| d.into())
            .collect::<Vec<_>>();
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Return {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let value = self.value.to_viper(ctx)?;
        let ass = ast.local_var_assign(ctx.return_var().1, value);
        let goto = ast.goto(ctx.return_label());

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(goto);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::If {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        let cond = self.cond.cond_to_viper(ctx)?;
        let mut then_ctx = ctx.child();
        let then_body = self.if_branch.to_viper(&mut then_ctx)?;
        let mut else_ctx = then_ctx.child();
        let else_body = self.else_branch.to_viper(&mut else_ctx)?;

        let decls = ctx.pop_decls();

        ctx.stack.push(ast.if_stmt(cond, then_body, else_body));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::While {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        let cond = self.cond.cond_to_viper(ctx)?;
        let mut body_ctx = ctx.child();
        body_ctx.enter_new_loop();
        let body = self.body.to_viper(&mut body_ctx)?;

        let decls = ctx
            .declarations
            .drain(..)
            .map(|d| d.into())
            .collect::<Vec<_>>();

        let mut body_seq = ctx.stack.clone();
        body_seq.push(body);
        body_seq.push(ast.label(&ctx.current_continue_label(), &[]));
        let body = ast.seqn(&body_seq, &[]);

        ctx.stack
            .push(ast.while_stmt(cond, &body_ctx.invariants, body));
        ctx.stack.push(ast.label(&ctx.current_break_label(), &[]));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Seq {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let stmts = self
            .stmts
            .into_iter()
            .map(|s| s.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ast.seqn(&stmts, &[]))
    }
}

impl<'a> TryToViper<'a> for ir::Definition {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let shape = self.rhs.to_shape(&ctx.typectx_get_mut())?;
        let var = ast.new_var(&self.lhs, shape.to_viper_type(ctx));
        ctx.declarations.push(var.0);

        // ctx.set_type(name, shape);
        let ass = ast.local_var_assign(var.1, self.rhs.to_viper(ctx)?);
        let scope = self.scope.to_viper(&mut ctx.child())?;

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(scope);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Assign {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let lhs_shape = ctx.get_type(&self.lhs);
        let rhs_shape = self.rhs.to_shape(&ctx.typectx_get_mut())?;
        if lhs_shape != rhs_shape {
            return Err(ToViperError::MismatchedShapes(lhs_shape, rhs_shape));
        }
        let var = ast.new_var(&self.lhs, lhs_shape.to_viper_type(ctx));

        let ass = ast.local_var_assign(var.1, self.rhs.to_viper(ctx)?);
        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Call {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        ir::Definition {
            lhs: ctx.fresh_varname(),
            rhs: self.call,
            scope: Box::new(ir::Stmt::Skip),
        }
        .to_viper(ctx)
    }
}

impl<'a> TryToViper<'a> for ir::ExtCall {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let args = self.args.to_viper(ctx)?;
        Ok(ast.method_call(&format!("ffi{}", self.fname), &args, &[]))
    }
}

impl<'a> TryToViper<'a> for ir::Annotation {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        use crate::ir::AnnotationType::*;
        match self.typ {
            fold @ (Fold | Unfold) => match self.expr {
                ir::Expr::FunctionCall(access) => {
                    let ast_node = |e| match fold {
                        Unfold => ast.unfold(e),
                        Fold => ast.fold(e),
                        _ => unreachable!(),
                    };
                    let mut args = access.args.to_viper(ctx)?;
                    args.insert(0, ctx.heap_var().1);
                    Ok(ast_node(ast.predicate_access_predicate(
                        ast.predicate_access(&args, &access.fname),
                        ast.full_perm(),
                    )))
                }
                _ => Err(ToViperError::InvalidFold(self.expr)),
            },
            x => {
                let no_pos = ast.no_position();

                ctx.set_mode(self.typ.into());
                let body = self.expr.to_viper(ctx)?;
                ctx.set_mode(TranslationMode::Normal);
                ctx.mangler_get_mut().clear_annot_var();
                Ok(match x {
                    Assertion => ast.assert(body, no_pos),
                    Assumption | Inhale => ast.inhale(body, no_pos),
                    Exhale => ast.exhale(body, no_pos),
                    Refutation => ast.refute(body, no_pos),
                    x @ (Invariant | Precondition | Postcondition) => {
                        match x {
                            Invariant => ctx.invariants.push(body),
                            Precondition => ctx.pres.push(body),
                            Postcondition => ctx.posts.push(body),
                            _ => unreachable!(),
                        };
                        ast.comment("annotation pushed")
                    }
                    _ => unreachable!(),
                })
            }
        }
    }
}
