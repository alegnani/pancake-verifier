use crate::{
    pancake_ast::{PancakeExpr, PancakeFnDec, PancakeOpType, PancakeStmt},
    viper_ast::{ViperArgDecl, ViperExpr, ViperMember, ViperMethod, ViperOp, ViperStmt, ViperType},
};

static RETURN_VAR: &str = "res";
static RETURN_LABEL: &str = "return_label";

fn mangle_var(varname: &str) -> String {
    format!("_{}", varname)
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

fn translate_stmt(pstmt: &PancakeStmt) -> ViperStmt {
    match pstmt {
        PancakeStmt::Assign(name, expr) => {
            ViperStmt::VarAssign(mangle_var(name), translate_expr(expr))
        }
        PancakeStmt::Break => todo!(),
        PancakeStmt::Call(handler, name, args) => {
            let fname = if let PancakeExpr::Label(n) = name {
                n
            } else {
                "error f-pointer"
            };
            ViperStmt::MethodCall(vec![], fname.to_string(), translate_exprs(args))
        }
        PancakeStmt::Continue => todo!(),
        PancakeStmt::Dec(name, expr, skip) => {
            // add assertions
            assert!(matches!(**skip, PancakeStmt::Skip));
            ViperStmt::VarDecl(mangle_var(name), ViperType::Int)
        }
        PancakeStmt::ExtCall(name, arg0, arg1, arg2, arg3) => ViperStmt::MethodCall(
            vec![],
            format!("ffi{}", name),
            translate_exprs(&[arg0.clone(), arg1.clone(), arg2.clone(), arg3.clone()]),
        ),
        PancakeStmt::If(cond, ifb, elseb) => ViperStmt::If(
            translate_expr(cond),
            Box::new(translate_stmt(ifb)),
            Box::new(translate_stmt(elseb)),
        ),
        PancakeStmt::Raise(a, b) => todo!(),
        PancakeStmt::Return(expr) => ViperStmt::Seq(vec![
            ViperStmt::VarAssign(RETURN_VAR.into(), translate_expr(expr)),
            ViperStmt::Goto(RETURN_LABEL.into()),
        ]),
        PancakeStmt::Seq(seq) => ViperStmt::Seq(translate_stmts(seq)),
        PancakeStmt::Skip => ViperStmt::Skip,
        PancakeStmt::Store(lhs, rhs) => todo!(),
        PancakeStmt::StoreByte(lhs, rhs) => todo!(),
        PancakeStmt::Tick => todo!(),
        PancakeStmt::While(cond, body) => {
            ViperStmt::While(translate_expr(cond), vec![], Box::new(translate_stmt(body)))
        }
    }
}

fn translate_stmts(pexprs: &[PancakeStmt]) -> Vec<ViperStmt> {
    pexprs.iter().map(translate_stmt).collect()
}

pub fn translate_fndec(fdec: &PancakeFnDec) -> ViperMember {
    let args = fdec
        .args
        .iter()
        .map(|a| ViperArgDecl(a.clone(), ViperType::Int))
        .collect();

    let body = Some(ViperStmt::Seq(vec![
        translate_stmt(&fdec.body),
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
