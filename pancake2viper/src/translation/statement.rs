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
        let body = ast.seqn(&body_seq, &[]);

        ctx.stack.push(ast.while_stmt(cond, &[], body));
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

// FIME: fix shadowing of variables
impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Declaration {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let decl = ast.local_var_decl(&ctx.mangle_var(&self.lhs), ast.int_type());
        ctx.declarations.push(decl);

        let var = ast.local_var(&ctx.mangle_var(&self.lhs), ast.int_type());
        let ass = ast.local_var_assign(var, self.rhs.to_viper(ctx));
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

// fn translate_stmt(mut ctx: TranslationContext, pstmt: &Stmt) -> ViperStmt {
//     println!("stmt: {:?}", pstmt);
//     match pstmt {
//         Stmt::Assign(Assign { lhs, rhs }) => {
//             ViperStmt::VarAssign(mangle_var(lhs), translate_expr(rhs))
//         }
//         Stmt::Break => ViperStmt::Goto(mangle_break(ctx)),
//         Stmt::Call(Call {
//             rettype,
//             fname,
//             args,
//         }) => {
//             println!("Handler: {}", rettype);
//             ViperStmt::MethodCall(vec![], parse_fname(fname), translate_exprs(args))
//         }
//         Stmt::TailCall(TailCall { fname, args }) => ViperStmt::Seq(vec![
//             ViperStmt::MethodCall(
//                 vec![RETURN_VAR.into()],
//                 parse_fname(fname),
//                 translate_exprs(args),
//             ),
//             ViperStmt::Goto(RETURN_LABEL.into()),
//         ]),
//         Stmt::Continue => ViperStmt::Goto(mangle_continue(ctx)),
//         Stmt::Declaration(Declaration {
//             lhs,
//             rhs:
//                 Expr::Call(ExprCall {
//                     rettype: _,
//                     fname,
//                     args,
//                 }),
//             scope: _,
//         }) => ViperStmt::Seq(vec![
//             ViperStmt::VarDecl(mangle_var(lhs), ViperType::Int),
//             ViperStmt::MethodCall(
//                 vec![mangle_var(lhs)],
//                 parse_fname(fname),
//                 translate_exprs(args),
//             ),
//         ]),
//         Stmt::Declaration(Declaration { lhs, rhs, scope }) => {
//             // add assertions
//             assert!(matches!(**scope, Stmt::Skip));
//             ViperStmt::Seq(vec![
//                 ViperStmt::VarDecl(mangle_var(lhs), ViperType::Int),
//                 ViperStmt::VarAssign(mangle_var(lhs), translate_expr(rhs)),
//             ])
//         }
//         Stmt::ExtCall(ExtCall { fname, args }) => {
//             ViperStmt::MethodCall(vec![], format!("ffi{}", fname), translate_exprs(args))
//         }
//         Stmt::If(If {
//             cond,
//             if_branch,
//             else_branch,
//         }) => ViperStmt::If(
//             translate_cond(cond),
//             Box::new(translate_stmt(ctx, if_branch)),
//             Box::new(translate_stmt(ctx, else_branch)),
//         ),
//         Stmt::Raise(Raise { error, idk }) => todo!(),
//         Stmt::Return(Return { value }) => ViperStmt::Seq(vec![
//             ViperStmt::VarAssign(RETURN_VAR.into(), translate_expr(value)),
//             ViperStmt::Goto(RETURN_LABEL.into()),
//         ]),
//         Stmt::Seq(Seq { stmts }) => ViperStmt::Seq(translate_stmts(ctx, stmts)),
//         Stmt::Skip => ViperStmt::Skip,
//         Stmt::Store(Store { address, value }) => todo!(),
//         Stmt::StoreByte(StoreByte { address, value }) => todo!(),
//         Stmt::Tick => todo!(),
//         Stmt::While(While { cond, body }) => {
//             ctx.inc();
//             ViperStmt::Seq(vec![
//                 ViperStmt::While(
//                     translate_cond(cond),
//                     vec![],
//                     Box::new(ViperStmt::Seq(vec![
//                         ViperStmt::Label(mangle_continue(ctx)),
//                         translate_stmt(ctx, body),
//                     ])),
//                 ),
//                 ViperStmt::Label(mangle_break(ctx)),
//             ])
//         }
//         Stmt::DecCall => todo!(),
//     }
// }
