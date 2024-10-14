use viper::{BinOpBv, BvSize::BV64, UnOpBv};

use crate::ir;

use crate::utils::{Shape, ToViperError, TryToViper};

use super::utils::ViperEncodeCtx;

impl<'a> TryToViper<'a> for ir::Load {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let zero = ast.int_lit(0);
        let eight = ast.int_lit(8);
        let iarray = ctx.iarray;
        let addr_exp = self.address.to_viper(ctx)?;
        let word_addr = ast.div(addr_exp, eight);

        if self.assert && ctx.options.assert_aligned_accesses {
            // assert addr % 8 == 0
            let assertion = ast.assert(
                ast.eq_cmp(ast.module(addr_exp, eight), zero),
                ast.no_position(),
            );
            ctx.stack.push(assertion);
        }

        Ok(if self.shape.is_simple() {
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
        })
    }
}

impl<'a> TryToViper<'a> for ir::LoadByte {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let eight = ast.int_lit(8);

        let byte_address = self.address.clone().to_viper(ctx)?;
        let word_offset = ast.module(byte_address, eight);
        let byte_mask = ast.backend_bv64_lit(255);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(eight, word_offset));

        let load = ir::Expr::Load(ir::Load {
            shape: Shape::Simple,
            address: self.address,
            assert: false,
        })
        .to_viper(ctx)?;

        Ok(ast.backend_bv_to_int(
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
        ))
    }
}

impl<'a> TryToViper<'a> for ir::Store {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let zero = ast.int_lit(0);
        let eight = ast.int_lit(8);
        let iarray = ctx.iarray;
        let addr_expr = self.address.to_viper(ctx)?;

        // FIXME: change this to match word size
        // assert addr % 8 == 0
        let assertion = if ctx.options.assert_aligned_accesses {
            ast.assert(
                ast.eq_cmp(ast.module(addr_expr, eight), zero),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };

        let word_addr = ast.div(addr_expr, eight);
        let rhs_shape = self.value.to_shape(ctx)?;
        let rhs = self.value.to_viper(ctx)?;

        let store = if rhs_shape.is_simple() {
            ast.field_assign(iarray.access(ctx.heap_var().1, word_addr), rhs)
        } else {
            iarray.copy_slice_m(
                rhs,
                zero,
                ctx.heap_var().1,
                word_addr,
                ast.int_lit(rhs_shape.len() as i64),
            )
        };

        Ok(ast.seqn(&[assertion, store], &[]))
    }
}

// FIXME: change this to match word size
impl<'a> TryToViper<'a> for ir::StoreBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let bits = self.size.bits();
        let ast = ctx.ast;
        let iarray = ctx.iarray;
        let eight = ast.int_lit(8);
        let zero = ast.int_lit(0);

        let assertion = if ctx.options.assert_aligned_accesses && self.size.bits() != 8 {
            ast.assert(
                ast.eq_cmp(
                    ast.module(
                        self.address.clone().to_viper(ctx)?,
                        ast.int_lit(bits as i64 / 8),
                    ),
                    zero,
                ),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };

        let byte_address = self.address.to_viper(ctx)?;
        // TODO: this assumes that words are 64 bits (8 bytes)
        let word_offset = ast.module(byte_address, eight);
        let word_address = ast.sub(byte_address, word_offset);
        let bit_mask = ast.backend_bv64_lit(2_u64.pow(bits) - 1);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(eight, word_offset));
        let mask = ast.bv_binop(BinOpBv::BvShl, BV64, bit_mask, shift_amount);
        let inv_mask = ast.bv_unnop(UnOpBv::Not, BV64, mask);
        let value = ast.bv_binop(
            BinOpBv::BitAnd,
            BV64,
            bit_mask,
            ast.int_to_backend_bv(BV64, self.value.to_viper(ctx)?),
        );
        let value = ast.bv_binop(BinOpBv::BvShl, BV64, value, shift_amount);
        let old = ast.int_to_backend_bv(BV64, iarray.access(ctx.heap_var().1, word_address));
        let new = ast.bv_binop(
            BinOpBv::BitOr,
            BV64,
            ast.bv_binop(BinOpBv::BitAnd, BV64, old, inv_mask),
            ast.bv_binop(BinOpBv::BitAnd, BV64, value, mask),
        );
        let new = ast.backend_bv_to_int(BV64, new);
        let field_ass = ast.field_assign(iarray.access(ctx.heap_var().1, word_address), new);
        Ok(ast.seqn(&[assertion, field_ass], &[]))
    }
}

// TODO: how to model shared memory?
impl<'a> TryToViper<'a> for ir::SharedStore {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        Ok(ast.method_call(
            "shared_store",
            &[self.address.to_viper(ctx)?, self.value.to_viper(ctx)?],
            &[],
        ))
    }
}

// TODO: how to model shared memory?
impl<'a> TryToViper<'a> for ir::SharedStoreBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        Ok(ast.method_call(
            "shared_store",
            &[self.address.to_viper(ctx)?, self.value.to_viper(ctx)?],
            &[],
        ))
    }
}

impl<'a> TryToViper<'a> for ir::SharedLoad {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        Ok(ast.method_call(
            "shared_load",
            &[self.address.to_viper(ctx)?],
            &[self.dst.to_viper(ctx)?],
        ))
    }
}

impl<'a> TryToViper<'a> for ir::SharedLoadBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        Ok(ast.method_call(
            "shared_load",
            &[self.address.to_viper(ctx)?],
            &[self.dst.to_viper(ctx)?],
        ))
    }
}
