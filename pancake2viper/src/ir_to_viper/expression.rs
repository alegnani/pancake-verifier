use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::utils::{
    ExprTypeResolution, ForceToBool, Mangler, Shape, ToType, ToViper, ToViperError, ToViperType,
    TranslationMode, TryToShape, TryToViper, ViperEncodeCtx, ViperUtils,
};

use crate::ir::{self, BinOpType, Type};

impl<'a> ForceToBool<'a> for ir::Expr {
    type Output = viper::Expr<'a>;

    fn force_to_bool(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let is_annot = ctx.get_mode().is_annot();
        let typ = self.resolve_expr_type(is_annot, ctx.typectx_get_mut())?;
        let value = self.to_viper(ctx)?;
        Ok(match typ {
            Type::Int => ast.ne_cmp(value, ast.zero()),
            Type::Bool => value,
            x => panic!("Can't cast {:?} to `Bool`", x),
        })
    }
}

impl ir::Expr {
    // TODO: this could well be a function pointer. If we stick to only using
    // valid function addresses (no unholy pointer arithmetic) we can encode
    // this as a switch statement checking the expression against all possible
    // function addresses.
    pub fn label_to_viper(&self) -> String {
        match self {
            Self::Label(label) => label.to_owned(),
            _ => todo!("Probably calling a function pointer. These are not yet implemented."),
        }
    }
}

impl<'a> ForceToBool<'a> for Vec<ir::Expr> {
    type Output = Vec<viper::Expr<'a>>;

    fn force_to_bool(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        self.into_iter().map(|e| e.force_to_bool(ctx)).collect()
    }
}

impl<'a> TryToViper<'a> for ir::UnOp {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        use ir::UnOpType::*;
        Ok(match self.optype {
            Minus => ast.minus(self.right.to_viper(ctx)?),
            Neg => ast.not(self.right.force_to_bool(ctx)?),
        })
    }
}

fn translate_op<'a>(
    ast: viper::AstFactory<'a>,
    optype: BinOpType,
    left: viper::Expr<'a>,
    right: viper::Expr<'a>,
) -> viper::Expr<'a> {
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
        ViperNotEqual | PancakeNotEqual => ast.ne_cmp(left, right),
        ViperEqual | PancakeEqual => ast.eq_cmp(left, right),
        Lt | SignedLt => ast.lt_cmp(left, right), // FIXME: correctly handle signed vs unsigned
        Lte | SignedLte => ast.le_cmp(left, right),
        Gt | SignedGt => ast.gt_cmp(left, right),
        Gte | SignedGte => ast.ge_cmp(left, right),
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
}

impl<'a> TryToViper<'a> for ir::BinOp {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let is_annot = ctx.get_mode().is_annot();
        let type_ctx = ctx.typectx_get_mut();
        let left_type = self.left.resolve_expr_type(is_annot, type_ctx)?;
        let right_type = self.right.resolve_expr_type(is_annot, type_ctx)?;

        use BinOpType::*;
        let (left, right) = match self.optype {
            BoolOr | BoolAnd => (
                self.left.force_to_bool(ctx)?,
                self.right.force_to_bool(ctx)?,
            ),
            PancakeEqual | PancakeNotEqual if left_type != right_type => (
                self.left.force_to_bool(ctx)?,
                self.right.force_to_bool(ctx)?,
            ),
            _ => (self.left.to_viper(ctx)?, self.right.to_viper(ctx)?),
        };
        let binop = translate_op(ast, self.optype, left, right);
        let binop = if !is_annot {
            match self.optype {
                Add | Sub | Mul if ctx.options.bounded_arithmetic => {
                    ast.module(binop, ctx.word_values())
                }
                Lt | Lte | Gt | Gte | SignedLt | SignedLte | SignedGt | SignedGte
                | PancakeEqual | PancakeNotEqual => ast.cond_exp(binop, ast.one(), ast.zero()),
                _ => binop,
            }
        } else {
            binop
        };

        if !is_annot {
            let typ = self.optype.to_type(is_annot);
            let fresh = Mangler::fresh_varname();
            let fresh_var = ast.new_var(&fresh, typ.to_viper_type(ctx));
            ctx.set_type(fresh, typ);
            let ass = ast.local_var_assign(fresh_var.1, binop);
            ctx.declarations.push(fresh_var.0);
            ctx.stack.push(ass);

            if let TranslationMode::WhileCond = ctx.get_mode() {
                let assumption = ast.inhale(ast.eq_cmp(fresh_var.1, binop), ast.no_position());
                ctx.while_stack.push(assumption);
            }

            if ctx.options.check_overflows {
                let assertion = ast.assert(
                    ctx.utils.bounded_f(fresh_var.1, ctx.options.word_size),
                    ast.no_position(),
                );
                ctx.stack.push(assertion);
                if let TranslationMode::WhileCond = ctx.get_mode() {
                    ctx.while_stack.push(assertion);
                }
            }
            Ok(fresh_var.1)
        } else {
            Ok(binop)
        }
    }
}

impl<'a> TryToViper<'a> for ir::Shift {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        use ir::ShiftType::*;
        let shift_type = match self.shifttype {
            Lsl => BinOpBv::BvShl,
            Asr => BinOpBv::BvAShr,
            Lsr => BinOpBv::BvLShr,
        };
        let value = ast.int_to_backend_bv(BV64, self.value.to_viper(ctx)?);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.int_lit(self.amount as i64));
        let shift = ast.bv_binop(shift_type, BV64, value, shift_amount);
        Ok(ast.backend_bv_to_int(BV64, shift))
    }
}

impl<'a> TryToViper<'a> for ir::Struct {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let type_ctx = ctx.type_ctx().clone();
        let elems = self
            .flatten()
            .into_iter()
            .flat_map(|e| e.flatten(&type_ctx))
            .flatten()
            .map(|e| e.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ast.explicit_seq(&elems))
    }
}

impl<'a> TryToViper<'a> for ir::Field {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let obj_shape = self.obj.to_shape(ctx.typectx_get_mut())?;
        let obj = self.obj.to_viper(ctx)?;

        Ok(match &obj_shape {
            Shape::Simple => unreachable!(),
            Shape::Nested(elems) => {
                if elems[self.field_idx].len() == 1 {
                    ast.seq_index(obj, ast.int_lit(self.field_idx as i64))
                } else {
                    let (offset, size) = obj_shape.access(self.field_idx)?;
                    ast.seq_drop(
                        ast.seq_take(obj, ast.int_lit((offset + size) as i64)),
                        ast.int_lit(offset as i64),
                    )
                }
            }
        })
    }
}

impl<'a> ToViper<'a> for ir::Decl {
    type Output = viper::LocalVarDecl<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        ast.local_var_decl(&self.name, self.typ.to_viper_type(ctx))
    }
}

impl<'a> TryToViper<'a> for ir::Quantified {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let vars = self
            .decls
            .into_iter()
            .map(|d| d.to_viper(ctx))
            .collect::<Vec<_>>();
        let triggers = if self.triggers.is_empty() {
            vec![]
        } else {
            vec![ast.trigger(&self.triggers.to_viper(ctx)?)]
        };
        Ok(ast.forall(&vars, &triggers, self.body.to_viper(ctx)?))
    }
}

impl<'a> TryToViper<'a> for ir::FunctionCall {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let args = self.args.to_viper(ctx)?;
        let mut base_args = ctx.get_default_args().1;
        Ok(match self.fname.as_str() {
            "f_alen" => {
                let arr = args[0];
                ctx.iarray.len_f(arr)
            }
            "f_old" => ast.old(args[0]),
            pred if ctx.is_predicate(pred) => {
                base_args.extend(args);
                ast.predicate_access_predicate(
                    ast.predicate_access(&base_args, pred),
                    ast.full_perm(),
                )
            }
            // Only used for model generation to remove "f_" prefix from predicate names
            pred if ctx.is_predicate(pred.trim_start_matches("f_")) => {
                base_args.extend(args);
                ast.predicate_access_predicate(
                    ast.predicate_access(&base_args, pred.trim_start_matches("f_")),
                    ast.full_perm(),
                )
            }
            "f_bounded" => ctx.utils.bounded_f(args[0], ctx.options.word_size),
            "f_bounded8" => ctx.utils.bounded_f(args[0], 8),
            "f_bounded16" => ctx.utils.bounded_f(args[0], 16),
            "f_bounded32" => ctx.utils.bounded_f(args[0], 32),
            "f_bounded64" => ctx.utils.bounded_f(args[0], 64),
            fname => {
                base_args.extend(args);

                let ret_type = ctx
                    .typectx_get()
                    .get_function_type(fname)?
                    .to_viper_type(ctx);
                ast.func_app(fname, &base_args, ret_type, ast.no_position())
            }
        })
    }
}

impl<'a> TryToViper<'a> for ir::MethodCall {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let ret = ast.new_var(
            &Mangler::fresh_varname(),
            ctx.get_type(&self.fname)?.to_viper_type(ctx),
        );
        ctx.consume_stack = false;

        // Transpiled arguments
        let args = self.args.to_viper(ctx)?;
        let mut base_args = ctx.get_default_args().1;
        base_args.extend(args);

        let call = ast.method_call(&self.fname, &base_args, &[ret.1]);
        ctx.declarations.push(ret.0);
        ctx.stack.push(call);
        ctx.consume_stack = true;

        Ok(ret.1)
    }
}

impl<'a> TryToViper<'a> for ir::ArrayAccess {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let idx = self.idx.to_viper(ctx)?;
        let typ = self
            .obj
            .resolve_expr_type(ctx.get_mode().is_annot(), ctx.typectx_get_mut())?;
        let obj = self.obj.to_viper(ctx)?;
        Ok(match typ {
            Type::Seq(_) => ctx.ast.seq_index(obj, idx),
            _ => ctx.iarray.access(obj, idx),
        })
    }
}

impl<'a> ToViper<'a> for ir::Permission {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        match self {
            Self::Write => ast.full_perm(),
            Self::Read => Self::Fractional(1, 2).to_viper(ctx),
            Self::Wildcard => ast.wildcard_perm(),
            Self::Fractional(numer, denom) => {
                ast.fractional_perm(ast.int_lit(numer), ast.int_lit(denom))
            }
        }
    }
}

impl<'a> TryToViper<'a> for ir::AccessPredicate {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let perm = self.perm.to_viper(ctx);
        // If specified as `acc(predicate(...))` turn into `predicate(...)` as the `acc` will be added later
        match *self.field {
            ir::Expr::FunctionCall(fcall) if ctx.is_predicate(&fcall.fname) => fcall.to_viper(ctx),
            field => Ok(ast.field_access_predicate(field.to_viper(ctx)?, perm)),
        }
    }
}

impl<'a> TryToViper<'a> for ir::UnfoldingIn {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        Ok(ast.unfolding(self.pred.to_viper(ctx)?, self.expr.to_viper(ctx)?))
    }
}

impl<'a> TryToViper<'a> for ir::Ternary {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let cond = self.cond.to_viper(ctx)?;
        let left = self.left.to_viper(ctx)?;
        let right = self.right.to_viper(ctx)?;
        Ok(ast.cond_exp(cond, left, right))
    }
}

impl<'a> TryToViper<'a> for ir::AccessSlice {
    type Output = viper::Expr<'a>;

    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let field = self.field.to_viper(ctx)?;
        let lower = self.lower.to_viper(ctx)?;
        let upper = self.upper.to_viper(ctx)?;
        let length = ast.add(
            ast.sub(upper, lower),
            ast.int_lit(match self.typ {
                ir::SliceType::Exclusive => 0,
                ir::SliceType::Inclusive => 1,
            }),
        );
        let perm = self.perm.to_viper(ctx);
        Ok(ctx.iarray.array_acc_expr(field, lower, length, perm))
    }
}

impl<'a> TryToViper<'a> for ir::ViperFieldAccess {
    type Output = viper::Expr<'a>;

    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let typ = ctx
            .type_ctx()
            .get_field_type(&self.field)?
            .to_viper_type(ctx);
        Ok(ast.field_access(self.obj.to_viper(ctx)?, ast.field(&self.field, typ)))
    }
}

impl<'a> TryToViper<'a> for ir::Expr {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        use ir::Expr::*;
        match self {
            UnOp(op) => op.to_viper(ctx),
            BinOp(op) => op.to_viper(ctx),
            FunctionCall(call) => call.to_viper(ctx),
            MethodCall(call) => call.to_viper(ctx),
            Shift(shift) => shift.to_viper(ctx),
            Field(field) => field.to_viper(ctx),
            Struct(struc) => struc.to_viper(ctx),
            ArrayAccess(heap) => heap.to_viper(ctx),
            Quantified(quant) => quant.to_viper(ctx),
            AccessPredicate(acc) => acc.to_viper(ctx),
            UnfoldingIn(u) => u.to_viper(ctx),
            Load(load) => load.to_viper(ctx),
            LoadBits(load) => load.to_viper(ctx),
            Ternary(ternary) => ternary.to_viper(ctx),
            AccessSlice(slice) => slice.to_viper(ctx),
            ViperFieldAccess(acc) => acc.to_viper(ctx),
            x => Ok(match x {
                Const(c) => ast.int_lit(c),
                BoolLit(b) if b => ast.true_lit(),
                BoolLit(b) if !b => ast.false_lit(),
                Var(name) if name == "result" => ast.result_with_pos(
                    ctx.get_type("result")?.to_viper_type(ctx),
                    ast.no_position(),
                ),
                Var(name) => ast.local_var(&name, ctx.get_type(&name)?.to_viper_type(ctx)),
                Label(_) => todo!(), // XXX: not sure if we need this
                BaseAddr => ast.zero(),
                BytesInWord => ast.int_lit(ctx.options.word_size as i64 / 8),
                Old(old) => ast.old(old.expr.to_viper(ctx)?),
                _ => unreachable!(),
            }),
        }
    }
}
