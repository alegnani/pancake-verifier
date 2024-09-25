use viper::BvSize::BV64;
use viper::{BinOpBv, Viper};

use crate::{pancake::Shape, utils::ViperUtils};

use crate::ir;

use crate::translation::{context::ViperEncodeCtx, ToShape, ToViper, ToViperType};

impl ir::Expr {
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

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::UnOp {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let right = self.right.to_viper(ctx);
        use ir::UnOpType::*;
        match self.optype {
            Minus => ast.minus(right),
            Neg => ast.not(right),
        }
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::BinOp {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let is_annot = ctx.options.is_annot;
        let translate_op = |optype, left, right| {
            let one = ast.int_lit(1);
            let zero = ast.int_lit(0);
            use ir::BinOpType::*;
            match optype {
                Add => ast.add(left, right),
                Sub => ast.sub(left, right),
                Mul => ast.mul(left, right),
                Div => ast.div(left, right),
                Modulo => ast.module(left, right),
                Imp => ast.implies(left, right),
                Iff => ast.eq_cmp(left, right),
                PancakeNotEqual if is_annot => ast.ne_cmp(left, right),
                PancakeNotEqual => ast.cond_exp(ast.ne_cmp(left, right), one, zero),
                PancakeEqual if is_annot => ast.eq_cmp(left, right),
                PancakeEqual => ast.cond_exp(ast.eq_cmp(left, right), one, zero),
                ViperNotEqual => ast.ne_cmp(left, right),
                ViperEqual => ast.eq_cmp(left, right),
                Lt => ast.cond_exp(ast.lt_cmp(left, right), one, zero),
                Lte => ast.cond_exp(ast.le_cmp(left, right), one, zero),
                Gt => ast.cond_exp(ast.gt_cmp(left, right), one, zero),
                Gte => ast.cond_exp(ast.ge_cmp(left, right), one, zero),
                BoolAnd => ast.and(left, right),
                BoolOr => ast.or(left, right),
                x => {
                    let lbv = ast.int_to_backend_bv(BV64, left);
                    let rbv = ast.int_to_backend_bv(BV64, right);
                    let bvop = match x {
                        BitAnd => ast.bv_binop(BinOpBv::BitAnd, BV64, lbv, rbv),
                        BitOr => ast.bv_binop(BinOpBv::BitOr, BV64, lbv, rbv),
                        BitXor => ast.bv_binop(BinOpBv::BitXor, BV64, lbv, rbv),
                        _ => unreachable!(),
                    };
                    ast.backend_bv_to_int(BV64, bvop)
                }
            }
        };
        translate_op(
            self.optype,
            self.left.to_viper(ctx),
            self.right.to_viper(ctx),
        )
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Shift {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        use ir::ShiftType::*;
        let shift_type = match self.shifttype {
            Lsl => BinOpBv::BvShl,
            Asr => BinOpBv::BvAShr,
            Lsr => BinOpBv::BvLShr,
        };
        let value = ast.int_to_backend_bv(BV64, self.value.to_viper(ctx));
        let shift_amount = ast.int_to_backend_bv(BV64, ast.int_lit(self.amount as i64));
        let shift = ast.bv_binop(shift_type, BV64, value, shift_amount);
        ast.backend_bv_to_int(BV64, shift)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Load {
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
            ctx.set_type(fresh_str, self.shape);

            let slice = iarray.create_slice_m(ctx.heap_var().1, word_addr, length, fresh);
            ctx.declarations.push(fresh_decl);
            ctx.stack.push(slice);
            fresh
        }
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::LoadByte {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let eight = ast.int_lit(8);

        let byte_address = self.address.clone().to_viper(ctx);
        let word_offset = ast.module(byte_address, eight);
        let byte_mask = ast.backend_bv64_lit(255);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(eight, word_offset));

        let load = ir::Expr::Load(ir::Load {
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

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Struct {
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

impl<'a> ToShape<'a> for ir::Struct {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let inner_shapes = self
            .elements
            .iter()
            .map(|e| e.shape(ctx))
            .collect::<Vec<_>>();
        Shape::Nested(inner_shapes)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Field {
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

impl<'a> ToShape<'a> for ir::Field {
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

impl<'a> ToViperType<'a> for ir::Type {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        let ast = ctx.ast;
        match self {
            Self::Bool => ast.bool_type(),
            Self::Int => ast.int_type(),
            Self::IArray => ctx.iarray.get_type(),
        }
    }
}

impl<'a> ToViper<'a, viper::LocalVarDecl<'a>> for ir::QuantifiedDecl {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::LocalVarDecl<'a> {
        let ast = ctx.ast;
        ast.local_var_decl(&self.name, self.typ.to_viper_type(ctx))
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Quantified {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        let vars = self
            .decls
            .into_iter()
            .map(|d| d.to_viper(ctx))
            .collect::<Vec<_>>();
        let triggers = self.triggers.to_viper(ctx);
        ast.forall(&vars, &[ast.trigger(&triggers)], self.body.to_viper(ctx))
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::FunctionCall {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        // FIXME: return type
        ast.func_app(
            &self.fname,
            &self.args.to_viper(ctx),
            ast.int_type(),
            ast.no_position(),
        )
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::HeapAccess {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let idx = self.idx.to_viper(ctx);
        let heap = ctx.heap_var().1;
        ctx.iarray.access(heap, idx)
    }
}

impl<'a> ToViper<'a, viper::Expr<'a>> for ir::Expr {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
        let ast = ctx.ast;
        use ir::Expr::*;
        match self {
            Const(c) => ast.int_lit(c),
            Var(name) => ast.local_var(
                ctx.mangle_var(&name),
                ctx.get_type(&name).to_viper_type(ctx),
            ),
            Label(_) => panic!(), // XXX: not sure if we need this
            UnOp(op) => op.to_viper(ctx),
            BinOp(op) => op.to_viper(ctx),
            FunctionCall(call) => call.to_viper(ctx),
            MethodCall(_) => panic!("Should only be possible as part of a DecCall"),
            Shift(shift) => shift.to_viper(ctx),
            Load(load) => load.to_viper(ctx),
            LoadByte(load) => load.to_viper(ctx),
            Field(field) => field.to_viper(ctx),
            BaseAddr => ast.int_lit(0),
            BytesInWord => ast.int_lit(64), // TODO: change this to actual word size
            Struct(struc) => struc.to_viper(ctx),
            Quantified(quant) => quant.to_viper(ctx),
            HeapAccess(heap) => heap.to_viper(ctx),
        }
    }
}

impl<'a> ToShape<'a> for ir::Expr {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        use ir::Expr::*;
        match self {
            Const(_) | UnOp(_) | BinOp(_) | Shift(_) | LoadByte(_) | Quantified(_)
            | HeapAccess(_) | BaseAddr | BytesInWord => Shape::Simple,
            Var(var) => ctx.get_type(var),
            MethodCall(call) => call.rettype.clone(),
            FunctionCall(call) => todo!(),
            Label(_) => panic!("Should not be possible"),
            Load(load) => load.shape.clone(),
            Field(field) => field.shape(ctx),
            Struct(struc) => struc.shape(ctx),
        }
    }
}

impl<'a> ToViper<'a, Vec<viper::Expr<'a>>> for Vec<ir::Expr> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Vec<viper::Expr<'a>> {
        self.into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>()
    }
}
