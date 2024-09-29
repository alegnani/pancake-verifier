use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::shape::Shape;
use crate::utils::ViperUtils;

use crate::ir::{self};

use crate::{ToShape, ToViper, ToViperType};

use super::utils::ViperEncodeCtx;

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

impl<'a> ToViper<'a> for ir::UnOp {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        let right = self.right.to_viper(ctx);
        use ir::UnOpType::*;
        match self.optype {
            Minus => ast.minus(right),
            Neg => ast.not(right),
        }
    }
}

impl<'a> ToViper<'a> for ir::BinOp {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        let is_annot = ctx.get_mode().is_annot();
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
                BoolAnd => ast.and(left, right),
                BoolOr => ast.or(left, right),
                ViperNotEqual => ast.ne_cmp(left, right),
                ViperEqual => ast.eq_cmp(left, right),

                x @ (PancakeNotEqual | PancakeEqual | Lt | Lte | Gt | Gte) => {
                    let cond = match x {
                        PancakeNotEqual => ast.ne_cmp(left, right),
                        PancakeEqual => ast.eq_cmp(left, right),
                        Lt => ast.lt_cmp(left, right),
                        Lte => ast.le_cmp(left, right),
                        Gt => ast.gt_cmp(left, right),
                        Gte => ast.ge_cmp(left, right),
                        _ => unreachable!(),
                    };
                    if is_annot {
                        cond
                    } else {
                        ast.cond_exp(cond, one, zero)
                    }
                }

                x @ (BitAnd | BitOr | BitXor) => {
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
        //         if ctx.options.expr_unrolling {
        //     let typ = ast.int_type();
        //     let fresh = ctx.fresh_var();
        //     let (var_decl, var) = ast.new_var(&fresh, typ);
        //     ctx.set_type(fresh, Shape::Simple);
        //     let right = expr.to_viper(ctx);
        //     let rhs = translate_op(self.optype, acc, right);
        //     let ass = ast.local_var_assign(var, rhs);
        //     ctx.declarations.push(var_decl);
        //     ctx.stack.push(ass);

        //     (ctx, var)
        // } else {
        translate_op(
            self.optype,
            self.left.to_viper(ctx),
            self.right.to_viper(ctx),
        )
    }
}

impl<'a> ToViper<'a> for ir::Shift {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
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

impl<'a> ToViper<'a> for ir::Struct {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
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

impl<'a> ToViper<'a> for ir::Field {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
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

impl<'a> ToViper<'a> for ir::Decl {
    type Output = viper::LocalVarDecl<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        ctx.set_type(self.name.clone(), Shape::Simple);
        ctx.mangler.new_annot_var(self.name.clone());
        ast.local_var_decl(&self.name, self.typ.to_viper_type(ctx))
    }
}

impl<'a> ToViper<'a> for ir::Quantified {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        let vars = self
            .decls
            .into_iter()
            .map(|d| d.to_viper(ctx))
            .collect::<Vec<_>>();
        let triggers = if self.triggers.is_empty() {
            vec![]
        } else {
            vec![ast.trigger(&self.triggers.to_viper(ctx))]
        };
        ast.forall(&vars, &triggers, self.body.to_viper(ctx))
    }
}

impl<'a> ToViper<'a> for ir::FunctionCall {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        // FIXME: return type
        match self.fname.as_str() {
            "alen" => {
                let arr = self.args[0].clone().to_viper(ctx);
                ctx.iarray.len_f(arr)
            }
            fname => ast.func_app(
                fname,
                &self.args.to_viper(ctx),
                ast.int_type(),
                ast.no_position(),
            ),
        }
    }
}

impl<'a> ToViper<'a> for ir::MethodCall {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        let ret = ast.new_var(&ctx.fresh_var(), self.rettype.to_viper_type(ctx));
        let method_name = ctx.mangler.mangle_fn(&self.fname.label_to_viper());

        let mut args = vec![];

        for arg in self.args {
            let arg = match arg.shape(ctx) {
                Shape::Simple => arg.to_viper(ctx),
                Shape::Nested(_) => {
                    let fresh = ast.new_var(&ctx.fresh_var(), arg.shape(ctx).to_viper_type(ctx));
                    let arg_len = arg.shape(ctx).len();
                    let viper_arg = arg.to_viper(ctx);
                    let copy_arg = ctx.iarray.create_slice_m(
                        viper_arg,
                        ast.int_lit(0),
                        ast.int_lit(arg_len as i64),
                        fresh.1,
                    );
                    ctx.declarations.push(fresh.0);
                    ctx.stack.push(copy_arg);
                    fresh.1
                }
            };
            args.push(arg);
        }
        args.insert(0, ctx.heap_var().1);

        let call = ast.method_call(&method_name, &args, &[ret.1]);
        ctx.declarations.push(ret.0);
        ctx.stack.push(call);
        ret.1
    }
}

impl<'a> ToViper<'a> for ir::HeapAccess {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let idx = self.idx.to_viper(ctx);
        let heap = ctx.heap_var().1;
        ctx.iarray.access(heap, idx)
    }
}

impl<'a> ToViper<'a> for ir::AccessPredicate {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        use crate::ir::Permission::*;
        let perm = match self.perm {
            Write => ast.full_perm(),
            Read | Wildcard => ast.wildcard_perm(),
            Fractional(numer, denom) => ast.fractional_perm(
                ir::Expr::Const(numer).to_viper(ctx),
                ir::Expr::Const(denom).to_viper(ctx),
            ),
        };
        ast.field_access_predicate(self.field.to_viper(ctx), perm)
    }
}

impl<'a> ToViper<'a> for ir::FieldAccessChain {
    type Output = viper::Expr<'a>;

    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        let obj_shape = self.obj.shape(ctx);

        let (final_shape, offset) =
            self.idxs
                .iter()
                .fold((obj_shape, 0), |(shape, padding), &idx| {
                    assert!(idx < shape.len(), "Field access out of bounds");
                    match &shape {
                        Shape::Simple => panic!("Can't acces field of shape '1'"),
                        Shape::Nested(elems) => {
                            let inner_shape = elems[idx].clone();
                            let offset = if inner_shape.is_simple() {
                                idx
                            } else {
                                shape.access(idx).0
                            };
                            (inner_shape, padding + offset)
                        }
                    }
                });
        assert!(
            matches!(final_shape, Shape::Simple),
            "Can't access field not of shape `1` in annotation, got {:?}",
            final_shape
        );
        let obj = self.obj.to_viper(ctx);
        let offset_exp = ast.int_lit(offset as i64);
        ctx.iarray.access(obj, offset_exp)
    }
}

impl<'a> ToViper<'a> for ir::Expr {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        use ir::Expr::*;
        match self {
            Const(c) => ast.int_lit(c),
            Var(name) => ast.local_var(
                ctx.mangler.mangle_var(&name),
                ctx.get_type(&name).to_viper_type(ctx),
            ),
            Label(_) => panic!(), // XXX: not sure if we need this
            UnOp(op) => op.to_viper(ctx),
            BinOp(op) => op.to_viper(ctx),
            FunctionCall(call) => call.to_viper(ctx),
            MethodCall(call) => call.to_viper(ctx),
            Shift(shift) => shift.to_viper(ctx),
            Load(load) => load.to_viper(ctx),
            LoadByte(load) => load.to_viper(ctx),
            Field(field) => field.to_viper(ctx),
            BaseAddr => ast.int_lit(0),
            BytesInWord => ast.int_lit(64), // TODO: change this to actual word size
            Struct(struc) => struc.to_viper(ctx),
            Quantified(quant) => quant.to_viper(ctx),
            HeapAccess(heap) => heap.to_viper(ctx),
            AccessPredicate(acc) => acc.to_viper(ctx),
            FieldAccessChain(f) => f.to_viper(ctx),
        }
    }
}

impl<'a> ToShape<'a> for ir::Expr {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        use ir::Expr::*;
        match self {
            Const(_) | UnOp(_) | BinOp(_) | Shift(_) | LoadByte(_) | Quantified(_)
            | HeapAccess(_) | AccessPredicate(_) | FieldAccessChain(_) | BaseAddr | BytesInWord => {
                Shape::Simple
            }
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
