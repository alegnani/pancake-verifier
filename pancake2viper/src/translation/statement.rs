use crate::pancake;

use super::top::{ToViper, ViperEncodeCtx};

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Stmt {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let stmt = match self {
            pancake::Stmt::Skip => ast.comment("skip"),
            pancake::Stmt::Declaration(dec) => dec.to_viper(ctx),
            pancake::Stmt::Assign(ass) => ass.to_viper(ctx),
            pancake::Stmt::Break => ast.goto(&ctx.current_break_label()),
            pancake::Stmt::Continue => ast.goto(&ctx.current_continue_label()),
            pancake::Stmt::Return(ret) => ret.to_viper(ctx),
            pancake::Stmt::If(ifs) => ifs.to_viper(ctx),
            pancake::Stmt::While(whiles) => whiles.to_viper(ctx),
            pancake::Stmt::Seq(seq) => seq.to_viper(ctx),
            pancake::Stmt::Call(call) => call.to_viper(ctx),
            pancake::Stmt::ExtCall(ext) => ext.to_viper(ctx),
            pancake::Stmt::TailCall(tail) => tail.to_viper(ctx),
            pancake::Stmt::Store(store) => store.to_viper(ctx),
            pancake::Stmt::StoreByte(store) => store.to_viper(ctx),
            _ => todo!(),
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

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Return {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let value = self.value.to_viper(ctx);
        let ass = ast.local_var_assign(ctx.return_var(), value);
        let goto = ast.goto(ctx.return_label());

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(goto);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::If {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;

        let mut then_ctx = ctx.child();
        let mut else_ctx = ctx.child();

        let cond = self.cond.cond_to_viper(ctx);
        let then_body = self.if_branch.to_viper(&mut then_ctx);
        let else_body = self.else_branch.to_viper(&mut else_ctx);

        let decls = ctx.pop_decls();

        ctx.stack.push(ast.if_stmt(cond, then_body, else_body));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::While {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;

        let mut body_ctx = ctx.child();

        let cond = self.cond.cond_to_viper(ctx);
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

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Seq {
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

// FIXME: fix shadowing of variables
impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Declaration {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let decl = ast.local_var_decl(&ctx.mangle_var(&self.lhs), ast.int_type());
        ctx.declarations.push(decl);

        let var = ast.local_var(&ctx.mangle_var(&self.lhs), ast.int_type());
        let ass = match self.rhs {
            pancake::Expr::Call(call) => {
                let args: Vec<viper::Expr> = call.args.to_viper(ctx);
                ast.method_call(&call.fname.label_to_viper(), &args, &[var])
            }
            other => ast.local_var_assign(var, other.to_viper(ctx)),
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

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Assign {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let var = ast.local_var(&ctx.mangle_var(&self.lhs), ast.int_type());
        let ass = ast.local_var_assign(var, self.rhs.to_viper(ctx));

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

// TODO: mangle function names
impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Call {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let decl = ast.local_var_decl("discard", ast.int_type());
        let var = ast.local_var("discard", ast.int_type());
        let args: Vec<viper::Expr> = self.args.to_viper(ctx);
        let call = ast.method_call(&self.fname.label_to_viper(), &args, &[var]);
        ast.seqn(&[call], &[decl.into()])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::ExtCall {
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

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::TailCall {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        let ret = ctx.return_var();
        let call = ast.method_call(&self.fname.label_to_viper(), &args, &[ret]);
        let goto = ast.goto(ctx.return_label());
        ast.seqn(&[call, goto], &[])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Store {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let addr_expr = self.address.to_viper(ctx);
        let f_app = ast.domain_func_app2(
            "slot",
            &[ctx.heap_var(), addr_expr],
            &[],
            ast.ref_type(),
            "IArray",
            ast.no_position(),
        );
        let elem_acc = ast.field_access(f_app, ast.field("heap_elem", ast.ref_type()));
        let rhs = self.value.to_viper(ctx);
        ast.field_assign(elem_acc, rhs)
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::StoreByte {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let truncate = pancake::Expr::Op(pancake::Op {
            optype: pancake::OpType::And,
            operands: vec![self.value, pancake::Expr::Const((1 << 8) - 1)],
        });
        ast.seqn(
            &[pancake::Stmt::Store(pancake::Store {
                address: self.address,
                value: truncate,
            })
            .to_viper(ctx)],
            &[],
        )
    }
}
