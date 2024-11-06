use viper::BinOpBv;
use viper::BvSize::BV64;

use crate::utils::{
    Mangler, Shape, ToType, ToViper, ToViperError, ToViperType, TranslationMode, TryToShape,
    TryToViper, ViperEncodeCtx, ViperUtils,
};

use crate::ir::{self, BinOpType, Type};

impl ir::Expr {
    pub fn cond_to_viper<'a>(
        self,
        ctx: &mut ViperEncodeCtx<'a>,
    ) -> Result<viper::Expr<'a>, ToViperError> {
        let ast = ctx.ast;
        if !self.to_shape(ctx.typectx_get_mut())?.is_simple() {
            return Err(ToViperError::ConditionShape(
                self.to_shape(ctx.typectx_get_mut())?,
            ));
        }
        ctx.set_mode(TranslationMode::WhileCond);
        let cond = self.to_viper(ctx)?;
        ctx.set_mode(TranslationMode::Normal);
        Ok(ast.ne_cmp(cond, ast.int_lit(0)))
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

impl<'a> TryToViper<'a> for ir::UnOp {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let right = self.right.to_viper(ctx)?;
        use ir::UnOpType::*;
        Ok(match self.optype {
            Minus => ast.minus(right),
            Neg => ast.not(right),
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
        ViperNotEqual => ast.ne_cmp(left, right),
        ViperEqual => ast.eq_cmp(left, right),
        PancakeNotEqual => ast.ne_cmp(left, right),
        PancakeEqual => ast.eq_cmp(left, right),
        Lt => ast.lt_cmp(left, right),
        Lte => ast.le_cmp(left, right),
        Gt => ast.gt_cmp(left, right),
        Gte => ast.ge_cmp(left, right),
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
        let binop = translate_op(
            ast,
            self.optype,
            self.left.to_viper(ctx)?,
            self.right.to_viper(ctx)?,
        );
        let binop = if !is_annot {
            use BinOpType::*;
            match self.optype {
                Add | Sub | Mul if ctx.options.bounded_arithmetic => {
                    ast.module(binop, ctx.word_values())
                }
                Lt | Lte | Gt | Gte | PancakeEqual | PancakeNotEqual => {
                    ast.cond_exp(binop, ast.one(), ast.zero())
                }
                _ => binop,
            }
        } else {
            binop
        };

        if ctx.options.expr_unrolling && !is_annot {
            let typ = if is_annot {
                self.optype.to_type()
            } else {
                Type::Int
            };
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
                let assertion = ast.assert(ctx.utils.bounded_f(fresh_var.1), ast.no_position());
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
        let shapes = self
            .elements
            .iter()
            .map(|e| e.to_shape(ctx.typectx_get_mut()))
            .collect::<Result<Vec<_>, _>>()?;
        let len: usize = shapes.iter().map(Shape::len).sum();
        let fresh = Mangler::fresh_varname();
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
        let assignments = self
            .flatten()
            .into_iter()
            .enumerate()
            .map(|(idx, e)| {
                let lhs = ctx.iarray.access(struct_var, ast.int_lit(idx as i64));
                e.to_viper(ctx).map(|rhs| ast.field_assign(lhs, rhs))
            })
            .collect::<Result<Vec<_>, _>>()?;
        assumptions.extend(assignments);
        ctx.declarations.push(struct_decl);
        ctx.stack.push(ast.seqn(&assumptions, &[]));
        Ok(struct_var)
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
                    ctx.iarray.access(obj, ast.int_lit(self.field_idx as i64))
                } else {
                    let fresh = Mangler::fresh_varname();
                    let (f_decl, f) = ast.new_var(&fresh, ctx.iarray.get_type());
                    ctx.declarations.push(f_decl);
                    let (offset, size) = obj_shape.access(self.field_idx)?;
                    ctx.stack.push(ctx.iarray.create_slice_m(
                        obj,
                        ast.int_lit(offset as i64),
                        ast.int_lit(size as i64),
                        f,
                    ));
                    f
                }
            }
        })
    }
}

impl<'a> ToViper<'a> for ir::Decl {
    type Output = viper::LocalVarDecl<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        let ast = ctx.ast;
        // XXX: move
        // ctx.set_type(self.name.clone(), self.typ.to_shape(ctx.typectx_get()));
        // ctx.mangler.new_annot_var(self.name.clone());
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
        let mut args = self.args.to_viper(ctx)?;
        Ok(match self.fname.as_str() {
            "f_alen" => {
                let arr = args[0];
                ctx.iarray.len_f(arr)
            }
            "f_old" => ast.old(args[0]),
            pred if ctx.is_predicate(pred) => {
                args.insert(0, ctx.heap_var().1);
                ast.predicate_access_predicate(ast.predicate_access(&args, pred), ast.full_perm())
            }
            "f_bounded" => ctx.utils.bounded_f(args[0]),
            fname => {
                args.insert(0, ctx.heap_var().1);
                let ret_type = ctx
                    .typectx_get()
                    .get_function_type(fname)?
                    .to_viper_type(ctx);
                ast.func_app(fname, &args, ret_type, ast.no_position())
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
        let mut args = vec![];

        for arg in self.args {
            let arg = match arg.to_shape(ctx.typectx_get_mut())? {
                Shape::Simple => arg.to_viper(ctx)?,
                Shape::Nested(_) => {
                    let fresh = ast.new_var(
                        &Mangler::fresh_varname(),
                        arg.to_shape(ctx.typectx_get_mut())?.to_viper_type(ctx),
                    );
                    let arg_len = arg.to_shape(ctx.typectx_get_mut())?.len();
                    let viper_arg = arg.to_viper(ctx)?;
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

        let call = ast.method_call(&self.fname, &args, &[ret.1]);
        ctx.declarations.push(ret.0);
        ctx.stack.push(call);
        Ok(ret.1)
    }
}

impl<'a> TryToViper<'a> for ir::ArrayAccess {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let obj = self.obj.to_viper(ctx)?;
        let idx = self.idx.to_viper(ctx)?;
        Ok(ctx.iarray.access(obj, idx))
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
        Ok(ast.field_access_predicate(self.field.to_viper(ctx)?, perm))
    }
}

impl<'a> TryToViper<'a> for ir::FieldAccessChain {
    type Output = viper::Expr<'a>;

    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let obj_shape = self.obj.to_shape(ctx.typectx_get_mut())?;

        let (final_shape, offset) =
            self.idxs
                .iter()
                .fold((obj_shape, 0), |(shape, padding), &idx| match &shape {
                    Shape::Simple => unreachable!(),
                    Shape::Nested(elems) => {
                        let inner_shape = elems[idx].clone();
                        let offset = if inner_shape.is_simple() {
                            idx
                        } else {
                            shape.access(idx).unwrap().0 // FIXME
                        };
                        (inner_shape, padding + offset)
                    }
                });
        if !final_shape.is_simple() {
            return Err(ToViperError::FieldAccessChainShape(final_shape));
        }

        let obj = self.obj.to_viper(ctx)?;
        let offset_exp = ast.int_lit(offset as i64);
        Ok(ctx.iarray.access(obj, offset_exp))
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
            FieldAccessChain(f) => f.to_viper(ctx),
            ArrayAccess(heap) => heap.to_viper(ctx),
            Quantified(quant) => quant.to_viper(ctx),
            AccessPredicate(acc) => acc.to_viper(ctx),
            UnfoldingIn(u) => u.to_viper(ctx),
            Load(load) => load.to_viper(ctx),
            LoadBits(load) => load.to_viper(ctx),
            Ternary(ternary) => ternary.to_viper(ctx),
            AccessSlice(slice) => slice.to_viper(ctx),
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
                BaseAddr => ast.int_lit(0),
                BytesInWord => ast.int_lit(ctx.options.word_size as i64 / 8),
                Old(old) => ast.old(old.expr.to_viper(ctx)?),
                _ => unreachable!(),
            }),
        }
    }
}
