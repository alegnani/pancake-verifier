use crate::ir;
use crate::utils::ViperUtils;

use crate::translation::{context::ViperEncodeCtx, ToShape, ToViper, ToViperType};

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Stmt {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        use ir::Stmt::*;
        let stmt = match self {
            Annotation(annot) => annot.to_viper(ctx),
            Skip => ast.comment("skip"),
            Definition(def) => def.to_viper(ctx),
            Assign(ass) => ass.to_viper(ctx),
            Break => ast.goto(&ctx.current_break_label()),
            Continue => ast.goto(&ctx.current_continue_label()),
            Return(ret) => ret.to_viper(ctx),
            If(ifs) => ifs.to_viper(ctx),
            While(whiles) => whiles.to_viper(ctx),
            Seq(seq) => seq.to_viper(ctx),
            Call(call) => call.to_viper(ctx),
            ExtCall(ext) => ext.to_viper(ctx),
            TailCall(tail) => tail.to_viper(ctx),
            Store(store) => store.to_viper(ctx),
            StoreBits(store) => store.to_viper(ctx),
            SharedStore(store) => store.to_viper(ctx),
            SharedStoreBits(store) => store.to_viper(ctx),
            SharedLoad(load) => load.to_viper(ctx),
            SharedLoadBits(load) => load.to_viper(ctx),
        };
        ctx.stack.push(stmt);

        let decls = ctx
            .declarations
            .drain(..)
            .map(|d| d.into())
            .collect::<Vec<_>>();
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Annotation {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        todo!()
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Return {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let value = self.value.to_viper(ctx);
        let ass = ast.local_var_assign(ctx.return_var().1, value);
        let goto = ast.goto(ctx.return_label());

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(goto);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::If {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;

        let cond = self.cond.cond_to_viper(ctx);
        let mut then_ctx = ctx.child();
        let then_body = self.if_branch.to_viper(&mut then_ctx);
        let mut else_ctx = then_ctx.child();
        let else_body = self.else_branch.to_viper(&mut else_ctx);

        let decls = ctx.pop_decls();

        ctx.stack.push(ast.if_stmt(cond, then_body, else_body));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::While {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;

        let cond = self.cond.cond_to_viper(ctx);
        let mut body_ctx = ctx.child();
        let body = self.body.to_viper(&mut body_ctx);

        let decls = ctx
            .declarations
            .drain(..)
            .map(|d| d.into())
            .collect::<Vec<_>>();

        let mut body_seq = ctx.stack.clone();
        body_seq.push(body);
        body_seq.push(ast.label(&ctx.current_continue_label(), &[]));
        let body = ast.seqn(&body_seq, &[]);

        ctx.stack.push(ast.while_stmt(cond, &[], body));
        ctx.stack.push(ast.label(&ctx.current_break_label(), &[]));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Seq {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let stmts = self
            .stmts
            .into_iter()
            .map(|s| s.to_viper(ctx))
            .collect::<Vec<_>>();
        ast.seqn(&stmts, &[])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Definition {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let name = ctx.mangler.new_scoped_var(self.lhs);
        let shape = self.rhs.shape(ctx);
        let var = ast.new_var(&name, shape.to_viper_type(ctx));
        ctx.declarations.push(var.0);

        ctx.set_type(name, shape);
        let ass = match self.rhs {
            ir::Expr::MethodCall(call) => {
                let mut args: Vec<viper::Expr> = call.args.to_viper(ctx);
                args.insert(0, ctx.heap_var().1);
                ast.method_call(
                    &ctx.mangler.mangle_fn(&call.fname.label_to_viper()),
                    &args,
                    &[var.1],
                )
            }
            other => ast.local_var_assign(var.1, other.to_viper(ctx)),
        };
        let scope = self.scope.to_viper(&mut ctx.child());

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(scope);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Assign {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let lhs_shape = ctx.get_type(&self.lhs);
        let name = ctx.mangler.mangle_var(&self.lhs);
        assert_eq!(lhs_shape, self.rhs.shape(ctx));
        let var = ast.local_var(name, lhs_shape.to_viper_type(ctx));
        let ass = ast.local_var_assign(var, self.rhs.to_viper(ctx));

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::Call {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        // FIXME: use actual return type
        let (decl, var) = ast.new_var("discard", ast.int_type());
        let mut args: Vec<viper::Expr> = self.args.to_viper(ctx);
        args.insert(0, ctx.heap_var().1);
        let call = ast.method_call(
            &ctx.mangler.mangle_fn(&self.fname.label_to_viper()),
            &args,
            &[var],
        );
        ast.seqn(&[call], &[decl.into()])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::ExtCall {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        ast.method_call(&format!("ffi_{}", self.fname), &args, &[])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for ir::TailCall {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let mut args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        args.insert(0, ctx.heap_var().1);
        let ret = ctx.return_var();
        let call = ast.method_call(
            &ctx.mangler.mangle_fn(&self.fname.label_to_viper()),
            &args,
            &[ret.1],
        );
        let goto = ast.goto(ctx.return_label());
        ast.seqn(&[call, goto], &[])
    }
}
