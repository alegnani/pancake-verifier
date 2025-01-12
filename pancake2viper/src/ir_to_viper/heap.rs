use viper::{BinOpBv, BvSize::BV64, UnOpBv};

use crate::ir;

use crate::ir::shared::SharedOpType::{Load, Store};
use crate::utils::{Shape, ToViperError, TryToShape, TryToViper, ViperEncodeCtx, ViperUtils};

impl<'a> TryToViper<'a> for ir::Load {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let bytes_in_word = ast.int_lit(ctx.options.word_size as i64 / 8);
        let iarray = ctx.iarray;
        let addr_exp = self.address.to_viper(ctx)?;
        let word_addr = ast.div(addr_exp, bytes_in_word);

        if self.assert && ctx.options.assert_aligned_accesses {
            // assert addr % @biw == 0
            let assertion = ast.assert(
                ast.eq_cmp(ast.module(addr_exp, bytes_in_word), ast.zero()),
                ast.no_position(),
            );
            ctx.stack.push(assertion);
        }

        Ok(if self.shape.is_simple() {
            iarray.access(ctx.heap_var().1, word_addr)
        } else {
            let length = self.shape.len() as i64;

            let elems = (0..length)
                .map(|offset| {
                    iarray.access(ctx.heap_var().1, ast.add(word_addr, ast.int_lit(offset)))
                })
                .collect::<Vec<_>>();
            ast.explicit_seq(&elems)
        })
    }
}

impl<'a> TryToViper<'a> for ir::LoadBits {
    type Output = viper::Expr<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let bytes_in_word = ast.int_lit(ctx.options.word_size as i64 / 8);

        let byte_address = self.address.clone().to_viper(ctx)?;
        let word_offset = ast.module(byte_address, bytes_in_word);
        let byte_mask = ast.backend_bv64_lit(2u64.pow(self.size.bits()) - 1);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(bytes_in_word, word_offset));

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
        let bytes_in_word = ast.int_lit(ctx.options.word_size as i64 / 8);
        let iarray = ctx.iarray;
        let addr_expr = self.address.to_viper(ctx)?;

        // assert addr % @biw == 0
        let assertion = if ctx.options.assert_aligned_accesses {
            ast.assert(
                ast.eq_cmp(ast.module(addr_expr, bytes_in_word), ast.zero()),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };

        let word_addr = ast.div(addr_expr, bytes_in_word);
        let rhs_shape = self.value.to_shape(ctx.typectx_get_mut())?;
        let rhs = self.value.to_viper(ctx)?;

        let store = if rhs_shape.is_simple() {
            ast.field_assign(iarray.access(ctx.heap_var().1, word_addr), rhs)
        } else {
            let length = rhs_shape.len() as i64;

            let elems = (0..length)
                .map(|offset| {
                    let src = ast.seq_index(rhs, ast.int_lit(offset));
                    let dst = ctx
                        .iarray
                        .access(ctx.heap_var().1, ast.add(addr_expr, ast.int_lit(offset)));
                    ast.local_var_assign(dst, src)
                })
                .collect::<Vec<_>>();
            ast.seqn(&elems, &[])
        };

        Ok(ast.seqn(&[assertion, store], &[]))
    }
}

impl<'a> TryToViper<'a> for ir::StoreBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let iarray = ctx.iarray;
        let bytes_in_word = ast.int_lit(ctx.options.word_size as i64 / 8);

        let assertion = if ctx.options.assert_aligned_accesses && self.size.bits() != 8 {
            ast.assert(
                ast.eq_cmp(
                    ast.module(
                        self.address.clone().to_viper(ctx)?,
                        ast.int_lit(self.size.bytes() as i64),
                    ),
                    ast.zero(),
                ),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };

        let byte_address = self.address.to_viper(ctx)?;
        let word_offset = ast.module(byte_address, bytes_in_word);
        let word_address = ast.sub(byte_address, word_offset);
        let bit_mask = ast.backend_bv64_lit(2_u64.pow(self.size.bits()) - 1);
        let shift_amount = ast.int_to_backend_bv(BV64, ast.mul(bytes_in_word, word_offset));
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

impl<'a> TryToViper<'a> for ir::SharedStore {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        ir::SharedStoreBits {
            address: self.address,
            value: self.value,
            size: ctx.options.word_size.into(),
        }
        .to_viper(ctx)
    }
}

impl<'a> TryToViper<'a> for ir::SharedStoreBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let addr_expr = self.address.clone().to_viper(ctx)?;
        // assert correct alignment
        let assertion = if ctx.options.assert_aligned_accesses && self.size.bytes() != 1 {
            ast.assert(
                ast.eq_cmp(
                    ast.module(addr_expr, ast.int_lit(self.size.bytes() as i64)),
                    ast.zero(),
                ),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };
        let value = self.value.to_viper(ctx)?;
        let mut args = ctx.get_default_args().1;
        args.push(addr_expr);
        args.push(value);
        if let Some(name) = &ctx.shared_override {
            let call = ast.seqn(
                &[
                    assertion,
                    ast.method_call(&format!("store_{}", name), &args, &[]),
                ],
                &[],
            );
            ctx.shared_override = None;
            return Ok(call);
        }
        match self.address {
            ir::Expr::Const(addr) => {
                let store_stmt = ast.method_call(
                    &ctx.shared
                        .get_method_name(addr, ctx.options, Store, self.size),
                    &args,
                    &[],
                );
                Ok(ast.seqn(&[assertion, store_stmt], &[]))
            }
            _ => Ok(ctx
                .shared
                .get_switch(ctx, addr_expr, Store, self.size, value)),
        }
    }
}

impl<'a> TryToViper<'a> for ir::SharedLoad {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        ir::SharedLoadBits {
            address: self.address,
            dst: self.dst,
            size: ctx.options.word_size.into(),
        }
        .to_viper(ctx)
    }
}

impl<'a> TryToViper<'a> for ir::SharedLoadBits {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let addr_expr = self.address.clone().to_viper(ctx)?;
        // assert correct alignment
        let assertion = if ctx.options.assert_aligned_accesses && self.size.bits() != 8 {
            ast.assert(
                ast.eq_cmp(
                    ast.module(addr_expr, ast.int_lit(self.size.bytes() as i64)),
                    ast.zero(),
                ),
                ast.no_position(),
            )
        } else {
            ast.comment("skipping alignment assertion")
        };
        let dst = self.dst.to_viper(ctx)?;
        let mut args = ctx.get_default_args().1;
        args.push(addr_expr);
        if let Some(name) = &ctx.shared_override {
            let call = ast.seqn(
                &[
                    assertion,
                    ast.method_call(&format!("load_{}", name), &args, &[dst]),
                ],
                &[],
            );
            ctx.shared_override = None;
            return Ok(call);
        }
        match &self.address {
            ir::Expr::Const(addr) => {
                let store_stmt = ast.method_call(
                    &ctx.shared
                        .get_method_name(*addr, ctx.options, Load, self.size),
                    &args,
                    &[dst],
                );
                Ok(ast.seqn(&[assertion, store_stmt], &[]))
            }
            _ => Ok(ctx.shared.get_switch(ctx, addr_expr, Load, self.size, dst)),
        }
    }
}
