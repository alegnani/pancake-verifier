use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::pancake::{self, ShiftType};

use super::top::{ToViper, ViperEncodeCtx};

impl pancake::Expr {
    pub fn cond_to_viper<'a>(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        match self {
            pancake::Expr::Op(op) => {
                if op.optype.is_bool() {
                    op.to_viper(ctx)
                } else {
                    let tmp = op.to_viper(ctx);
                    ast.ne_cmp(tmp, ast.int_lit(0))
                }
            }
            pancake::Expr::Const(c) => {
                let tmp = ast.int_lit(c);
                ast.ne_cmp(tmp, ast.int_lit(0))
            }
            x => x.to_viper(ctx),
        }
    }

    // TODO: this could well be a function pointer. If we stick to only using
    // valid function addresses (no unholy pointer arithmetic) we can encode
    // this as a switch statement checking the expression against all possible
    // function addresses.
    pub fn label_to_viper(&self) -> String {
        match self {
            Self::Label(label) => label.to_owned(),
            _ => panic!("Probably using f-pointer"),
        }
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Op {
    fn to_viper(mut self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        assert!(!self.operands.is_empty());
        let ast = ctx.ast;

        let init = self.operands.remove(0).to_viper(ctx);

        self.operands
            .into_iter()
            .fold((ctx, init), move |(ctx, acc), expr| {
                let typ = if self.optype.is_bool() {
                    ast.bool_type()
                } else {
                    ast.int_type()
                };
                let fresh = ctx.fresh_var();
                let var = ast.local_var(&fresh, typ);

                let decl = ast.local_var_decl(&fresh, typ);
                let right = expr.to_viper(ctx);
                let rhs = match self.optype {
                    pancake::OpType::Add => ast.add(acc, right),
                    pancake::OpType::Sub => ast.sub(acc, right),
                    pancake::OpType::Mul => ast.mul(acc, right),
                    pancake::OpType::NotEqual => ast.ne_cmp(acc, right),
                    pancake::OpType::Equal => ast.eq_cmp(acc, right),
                    pancake::OpType::Less => ast.lt_cmp(acc, right),
                    pancake::OpType::NotLess => ast.ge_cmp(acc, right),
                    x => {
                        let lbv = ast.int_to_backend_bv(BV64, acc);
                        let rbv = ast.int_to_backend_bv(BV64, right);
                        let bvop = match x {
                            pancake::OpType::And => ast.bv_binop(BinOpBv::BitAnd, BV64, lbv, rbv),
                            pancake::OpType::Or => ast.bv_binop(BinOpBv::BitOr, BV64, lbv, rbv),
                            pancake::OpType::Xor => ast.bv_binop(BinOpBv::BitXor, BV64, lbv, rbv),
                            _ => panic!("This is impossibl"),
                        };
                        ast.backend_bv_to_int(BV64, bvop)
                    }
                };
                let ass = ast.local_var_assign(var, rhs);
                ctx.declarations.push(decl);
                ctx.stack.push(ass);

                (ctx, var)
            })
            .1
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Shift {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let shift_type = match self.shifttype {
            ShiftType::Lsl => BinOpBv::BvShl,
            ShiftType::Asr => BinOpBv::BvAShr,
            ShiftType::Lsr => BinOpBv::BvLShr,
        };
        let value = ast.int_to_backend_bv(BV64, self.value.to_viper(ctx));
        let shift_amount = ast.int_to_backend_bv(BV64, ast.int_lit(self.amount as i64));
        let shift = ast.bv_binop(shift_type, BV64, value, shift_amount);
        ast.backend_bv_to_int(BV64, shift)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Expr {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        match self {
            pancake::Expr::Const(c) => ast.int_lit(c),
            pancake::Expr::Var(name) => ast.local_var(&ctx.mangle_var(&name), ast.int_type()), // FIXME: types
            pancake::Expr::Label(name) => todo!(), // not sure if we need this
            pancake::Expr::Op(op) => op.to_viper(ctx),
            pancake::Expr::Call(_) => panic!("Should only be possible as part of a DecCall"),
            pancake::Expr::Shift(shift) => shift.to_viper(ctx),
            _ => todo!(),
        }
    }
}

impl<'a> ToViper<'a, Vec<viper::Expr<'a>>> for Vec<pancake::Expr> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Vec<viper::Expr<'a>> {
        self.into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>()
    }
}
