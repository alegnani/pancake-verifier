use viper::{BinOpBv, BvSize::BV64, UnOpBv};

use crate::{pancake, utils::ViperUtils};

use super::top::{ToShape, ToViper, ToViperType, ViperEncodeCtx};

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Stmt {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        ctx.stack
            .insert(0, ast.comment(&format!("START: {:?}", &self)));
        let stmt = match self {
            pancake::Stmt::Annotation(annot) => ast.comment(&annot.line),
            pancake::Stmt::Skip => ast.comment("skip"),
            pancake::Stmt::Tick => ast.comment("tick"),
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
            pancake::Stmt::Raise(_) => todo!("Raise not implemented"),
        };
        ctx.stack.push(stmt);
        ctx.stack.push(ast.comment(" END "));

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
        let name = ctx.mangle_var(&self.lhs);
        let shape = self.rhs.shape(ctx);
        let decl = ast.local_var_decl(&name, shape.to_viper_type(ctx));
        ctx.declarations.push(decl);

        let var = ast.local_var(&ctx.mangle_var(&self.lhs), shape.to_viper_type(ctx));
        ctx.type_map.insert(name, shape);
        let ass = match self.rhs {
            pancake::Expr::Call(call) => {
                let mut args: Vec<viper::Expr> = call.args.to_viper(ctx);
                args.insert(0, ctx.heap_var().1);
                ast.method_call(&ctx.mangle_fn(&call.fname.label_to_viper()), &args, &[var])
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
        let name = ctx.mangle_var(&self.lhs);
        let lhs_shape = ctx.type_map.get(&name).unwrap();
        assert_eq!(lhs_shape, &self.rhs.shape(ctx));
        let var = ast.local_var(&ctx.mangle_var(&self.lhs), lhs_shape.to_viper_type(ctx));
        let ass = ast.local_var_assign(var, self.rhs.to_viper(ctx));

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        seq
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Call {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        // FIXME: use actual return type
        let (decl, var) = ast.new_var("discard", ast.int_type());
        let mut args: Vec<viper::Expr> = self.args.to_viper(ctx);
        args.insert(0, ctx.heap_var().1);
        let call = ast.method_call(&ctx.mangle_fn(&self.fname.label_to_viper()), &args, &[var]);
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
        let mut args = self
            .args
            .into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>();
        args.insert(0, ctx.heap_var().1);
        let ret = ctx.return_var();
        let call = ast.method_call(
            &ctx.mangle_fn(&self.fname.label_to_viper()),
            &args,
            &[ret.1],
        );
        let goto = ast.goto(ctx.return_label());
        ast.seqn(&[call, goto], &[])
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Store {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let zero = ast.int_lit(0);
        let eight = ast.int_lit(8);
        let iarray = ctx.iarray;
        let addr_expr = self.address.to_viper(ctx);

        // FIXME: change this to match word size
        // assert addr % 8 == 0
        let assertion = ast.assert(
            ast.eq_cmp(ast.module(addr_expr, eight), zero),
            ast.no_position(),
        );

        let word_addr = ast.div(addr_expr, eight);
        let rhs_shape = self.value.shape(ctx);
        let rhs = self.value.to_viper(ctx);

        let store = if rhs_shape.len() == 1 {
            ast.field_assign(iarray.access(ctx.heap_var().1, word_addr), rhs)
        } else {
            iarray.copy_slice_m(
                rhs,
                zero,
                ctx.heap_var().1,
                word_addr,
                ast.int_lit(rhs_shape.len() as i64),
            )
        };

        ast.seqn(&[assertion, store], &[])
    }
}

// FIXME: change this to match word size
impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::StoreByte {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let iarray = ctx.iarray;
        let eight = ast.int_lit(8);

        let byte_address = self.address.to_viper(ctx);
        let word_offset = ast.module(byte_address, eight);
        let word_address = ast.sub(byte_address, word_offset);
        let byte_mask = ast.backend_bv64_lit(255);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(eight, word_offset));
        let mask = ast.bv_binop(BinOpBv::BvShl, BV64, byte_mask, shift_amount);
        let inv_mask = ast.bv_unnop(UnOpBv::Not, BV64, mask);
        let value = ast.bv_binop(
            BinOpBv::BitAnd,
            BV64,
            byte_mask,
            ast.int_to_backend_bv(BV64, self.value.to_viper(ctx)),
        );
        let value = ast.bv_binop(BinOpBv::BvShl, BV64, value, shift_amount);
        let old = ast.int_to_backend_bv(BV64, iarray.access(ctx.heap_var().1, word_address));
        let new = ast.bv_binop(
            BinOpBv::BitOr,
            BV64,
            ast.bv_binop(BinOpBv::BitAnd, BV64, old, inv_mask),
            ast.bv_binop(BinOpBv::BitAnd, BV64, value, mask),
        );
        let new = ast.backend_bv_to_int(BV64, new);
        ast.field_assign(iarray.access(ctx.heap_var().1, word_address), new)
    }
}
