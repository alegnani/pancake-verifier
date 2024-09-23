use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::{
    pancake::{self, Shape, ShiftType},
    utils::ViperUtils,
};

use super::{
    context::ViperEncodeCtx,
    top::{ToShape, ToViper, ToViperType},
};

impl pancake::Expr {
    pub fn cond_to_viper<'a>(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        assert!(
            self.shape(ctx).is_simple(),
            "Can't use value of shape not `1` as condition"
        );
        ast.ne_cmp(self.to_viper(ctx), ast.int_lit(0))
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
        let translate_op = |optype, left, right| {
            let one = ast.int_lit(1);
            let zero = ast.int_lit(0);
            match optype {
                pancake::OpType::Add => ast.add(left, right),
                pancake::OpType::Sub => ast.sub(left, right),
                pancake::OpType::Mul => ast.mul(left, right),
                pancake::OpType::NotEqual => ast.cond_exp(ast.ne_cmp(left, right), one, zero),
                pancake::OpType::Equal => ast.cond_exp(ast.eq_cmp(left, right), one, zero),
                pancake::OpType::Less => ast.cond_exp(ast.lt_cmp(left, right), one, zero),
                pancake::OpType::NotLess => ast.cond_exp(ast.ge_cmp(left, right), one, zero),
                x => {
                    let lbv = ast.int_to_backend_bv(BV64, left);
                    let rbv = ast.int_to_backend_bv(BV64, right);
                    let bvop = match x {
                        pancake::OpType::And => ast.bv_binop(BinOpBv::BitAnd, BV64, lbv, rbv),
                        pancake::OpType::Or => ast.bv_binop(BinOpBv::BitOr, BV64, lbv, rbv),
                        pancake::OpType::Xor => ast.bv_binop(BinOpBv::BitXor, BV64, lbv, rbv),
                        _ => panic!("This is impossible"),
                    };
                    ast.backend_bv_to_int(BV64, bvop)
                }
            }
        };

        let init = self.operands.remove(0).to_viper(ctx);

        self.operands
            .into_iter()
            .fold((ctx, init), move |(ctx, acc), expr| {
                if ctx.options.expr_unrolling {
                    let typ = ast.int_type();
                    let fresh = ctx.fresh_var();
                    let (var_decl, var) = ast.new_var(&fresh, typ);
                    ctx.type_map.insert(fresh, Shape::Simple);
                    let right = expr.to_viper(ctx);
                    let rhs = translate_op(self.optype, acc, right);
                    let ass = ast.local_var_assign(var, rhs);
                    ctx.declarations.push(var_decl);
                    ctx.stack.push(ass);

                    (ctx, var)
                } else {
                    let new_expr = translate_op(self.optype, acc, expr.to_viper(ctx));
                    (ctx, new_expr)
                }
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
        let zero = ast.int_lit(0);
        let eight = ast.int_lit(8);
        let iarray = ctx.iarray;
        let addr_exp = self.address.to_viper(ctx);
        let word_addr = ast.div(addr_exp, eight);

        if self.assert && ctx.options.assert_aligned_accesses {
            // assert addr % 8 == 0
            let assertion = ast.assert(
                ast.eq_cmp(ast.module(addr_exp, eight), zero),
                ast.no_position(),
            );
            ctx.stack.push(assertion);
        }

        if self.shape.is_simple() {
            iarray.access(ctx.heap_var().1, word_addr)
        } else {
            let fresh_str = ctx.fresh_var();
            let (fresh_decl, fresh) = ast.new_var(&fresh_str, iarray.get_type());
            let length = ast.int_lit(self.shape.len() as i64);
            ctx.type_map.insert(fresh_str, self.shape);

            let slice = iarray.create_slice_m(ctx.heap_var().1, word_addr, length, fresh);
            ctx.declarations.push(fresh_decl);
            ctx.stack.push(slice);
            fresh
        }
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for pancake::LoadByte {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let eight = ast.int_lit(8);

        let byte_address = self.address.clone().to_viper(ctx);
        let word_offset = ast.module(byte_address, eight);
        let byte_mask = ast.backend_bv64_lit(255);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(eight, word_offset));

        let load = pancake::Expr::Load(pancake::Load {
            shape: Shape::Simple,
            address: self.address,
            assert: false,
        })
        .to_viper(ctx);

        ast.backend_bv_to_int(
            BV64,
            ast.bv_binop(
                BinOpBv::BitAnd,
                BV64,
                byte_mask,
                ast.bv_binop(
                    BinOpBv::BvLShr,
                    BV64,
                    ast.int_to_backend_bv(BV64, load),
                    shift_amount,
                ),
            ),
        )
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
                ctx.iarray.array_acc_expr(
                    struct_var,
                    ast.int_lit(0),
                    ctx.iarray.len_f(struct_var),
                    ast.full_perm(),
                ),
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
            pancake::Expr::BytesInWord => ast.int_lit(64), // TODO: change this to actual word size
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
            | pancake::Expr::BaseAddr
            | pancake::Expr::BytesInWord => Shape::Simple,
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
