use viper::{BinOpBv, BvSize::BV64, UnOpBv};

use crate::ir;

use crate::utils::{
    Mangler, Shape, ToType, ToViperError, TryToShape, TryToViper, ViperEncodeCtx, ViperUtils,
};

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
            let fresh_str = Mangler::fresh_varname();
            let (fresh_decl, fresh) = ast.new_var(&fresh_str, iarray.get_type());
            let length = ast.int_lit(self.shape.len() as i64);
            ctx.set_type(fresh_str, self.shape.to_type());

            let slice = iarray.create_slice_m(ctx.heap_var().1, word_addr, length, fresh);
            ctx.declarations.push(fresh_decl);
            ctx.stack.push(slice);
            fresh
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
            iarray.copy_slice_m(
                rhs,
                ast.zero(),
                ctx.heap_var().1,
                word_addr,
                ast.int_lit(rhs_shape.len() as i64),
            )
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

// TODO: how to model shared memory?
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

// TODO: how to model shared memory?
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
        match &self.address {
            ir::Expr::Const(addr) => {
                let value = self.value.to_viper(ctx)?;
                let store_stmt = ast.method_call(
                    &format!(
                        "store_{}",
                        ctx.shared.get_method_name(*addr as u64, self.size)
                    ),
                    &[ctx.heap_var().1, addr_expr, value],
                    &[],
                );
                Ok(ast.seqn(&[assertion, store_stmt], &[]))
            }
            _ => todo!(),
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
        match &self.address {
            ir::Expr::Const(addr) => {
                let dst = self.dst.to_viper(ctx)?;
                let store_stmt = ast.method_call(
                    &format!(
                        "load_{}",
                        ctx.shared.get_method_name(*addr as u64, self.size)
                    ),
                    &[ctx.heap_var().1, addr_expr],
                    &[dst],
                );
                Ok(ast.seqn(&[assertion, store_stmt], &[]))
            }
            _ => todo!(),
        }
    }
}
