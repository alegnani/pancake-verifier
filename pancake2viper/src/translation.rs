use crate::{
    pancake_ast::{PancakeExpr, PancakeFnDec, PancakeOpType, PancakeStmt},
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

fn translate_cond(cond: &PancakeExpr) -> ViperExpr {
    match cond {
        PancakeExpr::Op(op, _) if op.is_bool() => translate_expr(cond),
        x => ViperExpr::BinaryOp(
            Box::new(ViperExpr::IntLit(0)),
            ViperOp::Neq,
            Box::new(translate_expr(x)),
        ),
    }
}

fn translate_optype(typ: &PancakeOpType) -> ViperOp {
    match typ {
        PancakeOpType::Add => ViperOp::Add,
        PancakeOpType::And => todo!(), // ViperOp::And,
        PancakeOpType::Equal => ViperOp::Eq,
        PancakeOpType::Mul => ViperOp::Mul,
        PancakeOpType::NotEqual => ViperOp::Neq,
        PancakeOpType::Or => todo!(), //ViperOp::Or,
        PancakeOpType::Sub => ViperOp::Sub,
        PancakeOpType::Xor => todo!(),
        PancakeOpType::Less => ViperOp::Lt,
        PancakeOpType::NotLess => ViperOp::Gte,
    }
}

fn translate_expr(pexpr: &PancakeExpr) -> ViperExpr {
    match pexpr {
        PancakeExpr::BaseAddr => todo!(),
        PancakeExpr::Cmp(s, a, b) => todo!(),
        PancakeExpr::Const(num) => ViperExpr::IntLit(*num),
        PancakeExpr::Field(idx, obj) => todo!(),
        PancakeExpr::Label(label) => todo!(),
        PancakeExpr::Load(shape, addr) => todo!(),
        PancakeExpr::LoadByte(addr) => todo!(),
        PancakeExpr::Op(optype, operands) if operands.len() == 1 => ViperExpr::UnaryOp(
            translate_optype(optype),
            Box::new(translate_expr(&operands[0])),
        ),
        PancakeExpr::Op(optype, operands) => operands
            .iter()
            .map(translate_expr)
            .reduce(|acc, e| {
                ViperExpr::BinaryOp(Box::new(acc), translate_optype(optype), Box::new(e))
            })
            .unwrap(),
        PancakeExpr::Shift(typ, expr, amount) => todo!(),
        PancakeExpr::Struct(expr) => todo!(),
        PancakeExpr::Var(name) => ViperExpr::Var(mangle_var(name)),
        _ => todo!(),
    }
}

fn translate_exprs(pexprs: &[PancakeExpr]) -> Vec<ViperExpr> {
    pexprs.iter().map(translate_expr).collect()
}

fn translate_stmt(mut ctx: TranslationContext, pstmt: &PancakeStmt) -> ViperStmt {
    match pstmt {
        PancakeStmt::Assign(name, expr) => {
            ViperStmt::VarAssign(mangle_var(name), translate_expr(expr))
        }
        PancakeStmt::Break => ViperStmt::Goto(mangle_break(ctx)),
        PancakeStmt::Call(handler, name, args) => {
            let fname = if let PancakeExpr::Label(n) = name {
                n
            } else {
                "error f-pointer"
            };
            ViperStmt::MethodCall(vec![], fname.to_string(), translate_exprs(args))
        }
        PancakeStmt::Continue => ViperStmt::Goto(mangle_continue(ctx)),
        PancakeStmt::Dec(name, expr, skip) => {
            // add assertions
            assert!(matches!(**skip, PancakeStmt::Skip));
            ViperStmt::Seq(vec![
                ViperStmt::VarDecl(mangle_var(name), ViperType::Int),
                ViperStmt::VarAssign(mangle_var(name), translate_expr(expr)),
            ])
        }
        PancakeStmt::ExtCall(name, arg0, arg1, arg2, arg3) => ViperStmt::MethodCall(
            vec![],
            format!("ffi{}", name),
            translate_exprs(&[arg0.clone(), arg1.clone(), arg2.clone(), arg3.clone()]),
        ),
        PancakeStmt::If(cond, ifb, elseb) => ViperStmt::If(
            translate_cond(cond),
            Box::new(translate_stmt(ctx, ifb)),
            Box::new(translate_stmt(ctx, elseb)),
        ),
        PancakeStmt::Raise(a, b) => todo!(),
        PancakeStmt::Return(expr) => ViperStmt::Seq(vec![
            ViperStmt::VarAssign(RETURN_VAR.into(), translate_expr(expr)),
            ViperStmt::Goto(RETURN_LABEL.into()),
        ]),
        PancakeStmt::Seq(seq) => ViperStmt::Seq(translate_stmts(ctx, seq)),
        PancakeStmt::Skip => ViperStmt::Skip,
        PancakeStmt::Store(lhs, rhs) => todo!(),
        PancakeStmt::StoreByte(lhs, rhs) => todo!(),
        PancakeStmt::Tick => todo!(),
        PancakeStmt::While(cond, body) => {
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
    }
}

fn translate_stmts(ctx: TranslationContext, pexprs: &[PancakeStmt]) -> Vec<ViperStmt> {
    pexprs.iter().map(|e| translate_stmt(ctx, e)).collect()
}

pub fn translate_fndec(fdec: &PancakeFnDec) -> ViperMember {
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
        name: fdec.name.clone(),
        args,
        returns: vec![ViperArgDecl(RETURN_VAR.into(), ViperType::Int)],
        pre: vec![],
        post: vec![],
        body,
    };
    ViperMember::Method(vmethod)
}
