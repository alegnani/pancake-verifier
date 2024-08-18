use viper::AstFactory;

use crate::{
    pancake::{self, *},
    viper_ast::{ViperArgDecl, ViperExpr, ViperMember, ViperMethod, ViperOp, ViperStmt, ViperType},
};

static RETURN_VAR: &str = "res";
static RETURN_LABEL: &str = "return_label";
static BREAK_LABEL: &str = "break_label";
static CONTINUE_LABEL: &str = "continue_label";

#[derive(Debug, Default, Clone, Copy)]
struct TranslationContext {
    while_counter: u32,
}

impl TranslationContext {
    fn inc(&mut self) {
        self.while_counter += 1;
    }
}

fn mangle_continue(ctx: TranslationContext) -> String {
    format!("{}{}", CONTINUE_LABEL, ctx.while_counter)
}

fn mangle_break(ctx: TranslationContext) -> String {
    format!("{}{}", BREAK_LABEL, ctx.while_counter)
}

fn mangle_var(varname: &str) -> String {
    format!("_{}", varname)
}

fn parse_fname(name: &Expr) -> String {
    if let Expr::Label(n) = name {
        n.into()
    } else {
        "error f-pointer".into()
    }
}

fn translate_cond(cond: &Expr) -> ViperExpr {
    match cond {
        Expr::Op(Op {
            optype,
            operands: _,
        }) if optype.is_bool() => translate_expr(cond),
        x => ViperExpr::BinaryOp(
            Box::new(ViperExpr::IntLit(0)),
            ViperOp::Neq,
            Box::new(translate_expr(x)),
        ),
    }
}

fn translate_optype(typ: &OpType) -> ViperOp {
    match typ {
        OpType::Add => ViperOp::Add,
        OpType::And => todo!(), // ViperOp::And,
        OpType::Equal => ViperOp::Eq,
        OpType::Mul => ViperOp::Mul,
        OpType::NotEqual => ViperOp::Neq,
        OpType::Or => todo!(), //ViperOp::Or,
        OpType::Sub => ViperOp::Sub,
        OpType::Xor => todo!(),
        OpType::Less => ViperOp::Lt,
        OpType::NotLess => ViperOp::Gte,
    }
}

fn translate_expr(pexpr: &Expr) -> ViperExpr {
    println!("expr: {:?}", pexpr);
    match pexpr {
        Expr::BaseAddr => todo!(),
        Expr::Cmp(_, _, _) => panic!("PancakeExpr::Cmp not used"),
        Expr::Const(num) => ViperExpr::IntLit(*num),
        Expr::Field(Field { field_idx, obj }) => todo!(),
        Expr::Label(label) => panic!("{:?}", label),
        Expr::Load(Load { shape, address }) => todo!(),
        Expr::LoadByte(addr) => todo!(),
        Expr::Op(Op { optype, operands }) if operands.len() == 1 => ViperExpr::UnaryOp(
            translate_optype(optype),
            Box::new(translate_expr(&operands[0])),
        ),
        Expr::Op(Op { optype, operands }) => operands
            .iter()
            .map(translate_expr)
            .reduce(|acc, e| {
                ViperExpr::BinaryOp(Box::new(acc), translate_optype(optype), Box::new(e))
            })
            .unwrap(),
        Expr::Shift(Shift {
            shifttype,
            value,
            amount,
        }) => todo!(),
        Expr::Struct(expr) => translate_struct(&expr.0),
        Expr::Var(name) => ViperExpr::Var(mangle_var(name)),
        Expr::Call(_) => panic!("Should only be possible as part of a DecCall"),
    }
}

pub struct ViperEncodeCtx<'a> {
    pub ast: AstFactory<'a>,
    stack: Vec<viper::Stmt<'a>>,
    declarations: Vec<viper::LocalVarDecl<'a>>,
    fresh_counter: u64,
    while_counter: u64,
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self {
            ast,
            stack: vec![],
            declarations: vec![],
            fresh_counter: 0,
            while_counter: 0,
        }
    }

    pub fn fresh_var(&mut self) -> String {
        let fresh = format!("f_{}", self.fresh_counter);
        self.fresh_counter += 1;
        fresh
    }

    pub fn current_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter)
    }

    pub fn current_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter)
    }

    pub fn return_label(&self) -> &'static str {
        "return_label"
    }

    fn return_var_name(&self) -> &'static str {
        "retval"
    }

    pub fn return_decl(&self) -> viper::LocalVarDecl {
        self.ast
            .local_var_decl(self.return_var_name(), self.ast.int_type())
    }

    pub fn return_var(&self) -> viper::Expr {
        self.ast
            .local_var(self.return_var_name(), self.ast.int_type())
    }

    pub fn new_while_ctx(&mut self) {
        self.while_counter += 1;
    }

    pub fn push_stmt(&mut self, stmt: viper::Stmt<'a>) {
        self.stack.push(stmt)
    }
    pub fn push_decl(&mut self, decl: viper::LocalVarDecl<'a>) {
        self.declarations.push(decl)
    }
}

pub trait ToViper<'a, T> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> T;
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Op {
    fn to_viper(mut self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let ast_f = match self.optype {
            pancake::OpType::Add => AstFactory::add,
            pancake::OpType::Sub => AstFactory::sub,
            pancake::OpType::Mul => AstFactory::mul,
            pancake::OpType::Equal => AstFactory::eq_cmp,
            pancake::OpType::NotEqual => AstFactory::ne_cmp,
            pancake::OpType::Less => AstFactory::lt_cmp,
            pancake::OpType::NotLess => AstFactory::ge_cmp,
            x => todo!("{:?} not yet implemented", x),
        };
        assert!(!self.operands.is_empty());

        let init = self.operands.remove(0).to_viper(ctx);

        self.operands
            .into_iter()
            .fold((ctx, init), |(ctx, acc), expr| {
                let typ = ast.int_type();
                let fresh = ctx.fresh_var();
                let var = ast.local_var(&fresh, typ);

                let decl = ast.local_var_decl(&fresh, typ);
                let ass = ast.local_var_assign(var, ast_f(&ast, acc, expr.to_viper(ctx)));
                ctx.push_decl(decl);
                ctx.push_stmt(ass);

                (ctx, var)
            })
            .1
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Call {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        todo!()
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Expr {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        match self {
            pancake::Expr::Const(c) => ast.int_lit(c),
            pancake::Expr::Var(name) => ast.local_var(&name, ast.int_type()), // FIXME: types
            pancake::Expr::Label(name) => todo!(), // not sure if we need this
            pancake::Expr::Op(op) => op.to_viper(ctx),
            _ => todo!(),
        }
    }
}

impl<'a> ToViper<'a, viper::Stmt<'a>> for pancake::Stmt {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let stmt = match self {
            pancake::Stmt::Skip => ast.comment("skip"),
            // pancake::Stmt::Declaration(dec) => dec.to_viper(ctx),
            pancake::Stmt::Break => ast.goto(&ctx.current_break_label()),
            pancake::Stmt::Continue => ast.goto(&ctx.current_continue_label()),
            pancake::Stmt::Return(ret) => ret.to_viper(ctx),
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
        ast.seqn(&[ass, goto], &[])
    }
}

impl<'a> ToViper<'a, viper::Method<'a>> for pancake::FnDec {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Method<'a> {
        let ast = ctx.ast;
        let body = ast.seqn(
            &[self.body.to_viper(ctx), ast.label(ctx.return_label(), &[])],
            &[],
        );
        let args = self
            .args
            .iter()
            .map(|a| ast.local_var_decl(a, ast.int_type()))
            .collect::<Vec<_>>();
        ast.method(
            &self.fname,
            &args,
            &[ctx.return_decl()],
            &[],
            &[],
            Some(body),
        )
    }
}

impl<'a> ToViper<'a, viper::Program<'a>> for pancake::Program {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Program<'a> {
        let ast = ctx.ast;
        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_viper(ctx))
            .collect::<Vec<_>>();
        ast.program(&[], &[], &[], &[], &functions)
    }
}

fn translate_exprs(pexprs: &[Expr]) -> Vec<ViperExpr> {
    pexprs.iter().map(translate_expr).collect()
}

fn translate_struct(flat: &[Expr]) -> ViperExpr {
    todo!()
}

fn translate_stmt(mut ctx: TranslationContext, pstmt: &Stmt) -> ViperStmt {
    println!("stmt: {:?}", pstmt);
    match pstmt {
        Stmt::Assign(Assign { lhs, rhs }) => {
            ViperStmt::VarAssign(mangle_var(lhs), translate_expr(rhs))
        }
        Stmt::Break => ViperStmt::Goto(mangle_break(ctx)),
        Stmt::Call(Call {
            rettype,
            fname,
            args,
        }) => {
            println!("Handler: {}", rettype);
            ViperStmt::MethodCall(vec![], parse_fname(fname), translate_exprs(args))
        }
        Stmt::TailCall(TailCall { fname, args }) => ViperStmt::Seq(vec![
            ViperStmt::MethodCall(
                vec![RETURN_VAR.into()],
                parse_fname(fname),
                translate_exprs(args),
            ),
            ViperStmt::Goto(RETURN_LABEL.into()),
        ]),
        Stmt::Continue => ViperStmt::Goto(mangle_continue(ctx)),
        Stmt::Declaration(Declaration {
            lhs,
            rhs:
                Expr::Call(ExprCall {
                    rettype: _,
                    fname,
                    args,
                }),
            scope: _,
        }) => ViperStmt::Seq(vec![
            ViperStmt::VarDecl(mangle_var(lhs), ViperType::Int),
            ViperStmt::MethodCall(
                vec![mangle_var(lhs)],
                parse_fname(fname),
                translate_exprs(args),
            ),
        ]),
        Stmt::Declaration(Declaration { lhs, rhs, scope }) => {
            // add assertions
            assert!(matches!(**scope, Stmt::Skip));
            ViperStmt::Seq(vec![
                ViperStmt::VarDecl(mangle_var(lhs), ViperType::Int),
                ViperStmt::VarAssign(mangle_var(lhs), translate_expr(rhs)),
            ])
        }
        Stmt::ExtCall(ExtCall { fname, args }) => {
            ViperStmt::MethodCall(vec![], format!("ffi{}", fname), translate_exprs(args))
        }
        Stmt::If(If {
            cond,
            if_branch,
            else_branch,
        }) => ViperStmt::If(
            translate_cond(cond),
            Box::new(translate_stmt(ctx, if_branch)),
            Box::new(translate_stmt(ctx, else_branch)),
        ),
        Stmt::Raise(Raise { error, idk }) => todo!(),
        Stmt::Return(Return { value }) => ViperStmt::Seq(vec![
            ViperStmt::VarAssign(RETURN_VAR.into(), translate_expr(value)),
            ViperStmt::Goto(RETURN_LABEL.into()),
        ]),
        Stmt::Seq(Seq { stmts }) => ViperStmt::Seq(translate_stmts(ctx, stmts)),
        Stmt::Skip => ViperStmt::Skip,
        Stmt::Store(Store { address, value }) => todo!(),
        Stmt::StoreByte(StoreByte { address, value }) => todo!(),
        Stmt::Tick => todo!(),
        Stmt::While(While { cond, body }) => {
            ctx.inc();
            ViperStmt::Seq(vec![
                ViperStmt::While(
                    translate_cond(cond),
                    vec![],
                    Box::new(ViperStmt::Seq(vec![
                        ViperStmt::Label(mangle_continue(ctx)),
                        translate_stmt(ctx, body),
                    ])),
                ),
                ViperStmt::Label(mangle_break(ctx)),
            ])
        }
        Stmt::DecCall => todo!(),
    }
}

fn translate_stmts(ctx: TranslationContext, pexprs: &[Stmt]) -> Vec<ViperStmt> {
    pexprs.iter().map(|e| translate_stmt(ctx, e)).collect()
}

pub fn translate_fndec(fdec: &FnDec) -> ViperMember {
    let args = fdec
        .args
        .iter()
        .map(|a| ViperArgDecl(a.clone(), ViperType::Int))
        .collect();

    let ctx = TranslationContext::default();

    let body = Some(ViperStmt::Seq(vec![
        translate_stmt(ctx, &fdec.body),
        ViperStmt::Label(RETURN_LABEL.into()),
    ]));

    let vmethod = ViperMethod {
        name: fdec.fname.clone(),
        args,
        returns: vec![ViperArgDecl(RETURN_VAR.into(), ViperType::Int)],
        pre: vec![],
        post: vec![],
        body,
    };
    ViperMember::Method(vmethod)
}
