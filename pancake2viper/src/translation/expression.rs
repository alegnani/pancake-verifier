use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::{
    pancake::{self, Shape, ShiftType},
    utils::ViperUtils,
};

use super::top::{ToShape, ToViper, ToViperType, ViperEncodeCtx};

impl pancake::Expr {
    pub fn cond_to_viper<'a>(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        match self {
            pancake::Expr::Op(op) => {
                let tmp = op.to_viper(ctx);
                ast.ne_cmp(tmp, ast.int_lit(0))
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
                let typ = ast.int_type();
                let fresh = ctx.fresh_var();
                let (var_decl, var) = ast.new_var(&fresh, typ);
                ctx.type_map.insert(fresh, Shape::Simple);
                let right = expr.to_viper(ctx);
                let one = ast.int_lit(1);
                let zero = ast.int_lit(0);
                let rhs = match self.optype {
                    pancake::OpType::Add => ast.add(acc, right),
                    pancake::OpType::Sub => ast.sub(acc, right),
                    pancake::OpType::Mul => ast.mul(acc, right),
                    pancake::OpType::NotEqual => ast.cond_exp(ast.ne_cmp(acc, right), one, zero),
                    pancake::OpType::Equal => ast.cond_exp(ast.eq_cmp(acc, right), one, zero),
                    pancake::OpType::Less => ast.cond_exp(ast.lt_cmp(acc, right), one, zero),
                    pancake::OpType::NotLess => ast.cond_exp(ast.ge_cmp(acc, right), one, zero),
                    x => {
                        let lbv = ast.int_to_backend_bv(BV64, acc);
                        let rbv = ast.int_to_backend_bv(BV64, right);
                        let bvop = match x {
                            pancake::OpType::And => ast.bv_binop(BinOpBv::BitAnd, BV64, lbv, rbv),
                            pancake::OpType::Or => ast.bv_binop(BinOpBv::BitOr, BV64, lbv, rbv),
                            pancake::OpType::Xor => ast.bv_binop(BinOpBv::BitXor, BV64, lbv, rbv),
                            _ => panic!("This is impossible"),
                        };
                        ast.backend_bv_to_int(BV64, bvop)
                    }
                };
                let ass = ast.local_var_assign(var, rhs);
                ctx.declarations.push(var_decl);
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

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Load {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        // if self.shape
        let fresh_str = ctx.fresh_var();
        let array_type = ctx.iarray.get_type();
        let (fresh_decl, fresh) = ast.new_var(&fresh_str, array_type);
        let idx = self.address.to_viper(ctx);
        let length = ast.int_lit(self.shape.len() as i64);

        ctx.type_map.insert(fresh_str, self.shape);

        let slice = ctx
            .iarray
            .create_slice_m(ctx.heap_var().1, idx, length, fresh);
        ctx.declarations.push(fresh_decl);
        ctx.stack.push(slice);
        fresh
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::LoadByte {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        todo!()
        // pancake::Stmt::Seq(pancake::Seq { stmts: vec![

        // ] }).to_viper(ctx)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Struct {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let len: usize = self.elements.iter().map(|e| e.shape(ctx).len()).sum();
        let fresh = ctx.fresh_var();
        let (struct_decl, struct_var) = ast.new_var(&fresh, ctx.heap_type());
        let mut assumptions = vec![
            ast.inhale(
                ast.eq_cmp(ctx.iarray.len_f(struct_var), ast.int_lit(len as i64)),
                ast.no_position(),
            ),
            ast.inhale(
                ctx.iarray
                    .array_acc_expr(struct_var, ast.int_lit(0), ctx.iarray.len_f(struct_var)),
                ast.no_position(),
            ),
        ];
        let assignments = self.flatten().into_iter().enumerate().map(|(idx, e)| {
            let lhs = ctx.iarray.access(struct_var, ast.int_lit(idx as i64));
            ast.field_assign(lhs, e.to_viper(ctx))
        });
        assumptions.extend(assignments);
        ctx.declarations.push(struct_decl);
        ctx.stack.push(ast.seqn(&assumptions, &[]));
        struct_var
    }
}

impl<'a> ToShape<'a> for pancake::Struct {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let inner_shapes = self
            .elements
            .iter()
            .map(|e| e.shape(ctx))
            .collect::<Vec<_>>();
        Shape::Nested(inner_shapes)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Field {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let obj_shape = self.obj.shape(ctx);
        let obj = self.obj.to_viper(ctx);

        assert!(
            self.field_idx < obj_shape.len(),
            "Field access out of bounds"
        );
        match &obj_shape {
            Shape::Simple => panic!("Can't acces field of shape '1'"),
            Shape::Nested(elems) => {
                if elems[self.field_idx].len() == 1 {
                    ctx.iarray.access(obj, ast.int_lit(self.field_idx as i64))
                } else {
                    let fresh = ctx.fresh_var();
                    let (f_decl, f) = ast.new_var(&fresh, ctx.iarray.get_type());
                    ctx.declarations.push(f_decl);
                    let (offset, size) = obj_shape.access(self.field_idx);
                    ctx.stack.push(ctx.iarray.create_slice_m(
                        obj,
                        ast.int_lit(offset as i64),
                        ast.int_lit(size as i64),
                        f,
                    ));
                    f
                }
            }
        }
    }
}

impl<'a> ToShape<'a> for pancake::Field {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let obj_shape = self.obj.shape(ctx);
        match obj_shape {
            Shape::Simple => panic!("Field access into value of shape '1'"),
            Shape::Nested(ls) => {
                assert!(self.field_idx < ls.len(), "Field access out of bounds");
                ls[self.field_idx].clone()
            }
        }
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::Expr {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        match self {
            pancake::Expr::Const(c) => ast.int_lit(c),
            pancake::Expr::Var(name) => ast.local_var(
                &ctx.mangle_var(&name),
                ctx.type_map
                    .get(&ctx.mangle_var(&name))
                    .unwrap()
                    .to_viper_type(ctx),
            ),
            pancake::Expr::Label(_) => panic!(), // XXX: not sure if we need this
            pancake::Expr::Op(op) => op.to_viper(ctx),
            pancake::Expr::Call(_) => panic!("Should only be possible as part of a DecCall"),
            pancake::Expr::Shift(shift) => shift.to_viper(ctx),
            pancake::Expr::Load(load) => load.to_viper(ctx),
            pancake::Expr::LoadByte(load) => load.to_viper(ctx),
            pancake::Expr::Field(field) => field.to_viper(ctx),
            pancake::Expr::BaseAddr => ast.int_lit(0),
            pancake::Expr::Struct(struc) => struc.to_viper(ctx),
        }
    }
}

impl<'a> ToShape<'a> for pancake::Expr {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        match self {
            pancake::Expr::Const(_)
            | pancake::Expr::Op(_)
            | pancake::Expr::Shift(_)
            | pancake::Expr::LoadByte(_)
            | pancake::Expr::BaseAddr => Shape::Simple,
            pancake::Expr::Var(var) => ctx.type_map.get(&ctx.mangle_var(var)).unwrap().clone(),
            pancake::Expr::Call(call) => call.rettype.clone(),
            pancake::Expr::Label(_) => panic!("Should not be possible"),
            pancake::Expr::Load(load) => load.shape.clone(),
            pancake::Expr::Field(field) => field.shape(ctx),
            pancake::Expr::Struct(struc) => struc.shape(ctx),
        }
    }
}

impl<'a> ToViperType<'a> for Shape {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        match self {
            Self::Simple => ctx.ast.int_type(),
            Self::Nested(_) => ctx.iarray.get_type(),
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
