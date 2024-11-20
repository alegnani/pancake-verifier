use crate::utils::{EncodeOptions, ToViper, ToViperError, TryToViper, ViperEncodeCtx, ViperUtils};
use std::rc::Rc;

use super::{Expr, MemOpBytes, Shared, SharedPerm};
use std::{collections::HashSet, fmt::Display, option};

#[derive(Clone, Default)]
pub struct SharedContext {
    read_addresses: HashSet<i64>,
    write_addresses: HashSet<i64>,
    mappings: [Vec<SharedInternal>; 4],
}

#[derive(Debug, Clone)]
struct SharedInternal {
    name: String,
    typ: SharedPerm,
    size: MemOpBytes,
    addresses: Vec<i64>,
    lower: i64,
    upper: i64,
    stride: i64,
}

impl SharedInternal {
    pub fn get_precondition<'a>(
        &self,
        ctx: &ViperEncodeCtx<'a>,
        addr: viper::Expr<'a>,
    ) -> viper::Expr<'a> {
        assert!(!self.addresses.is_empty());
        let ast = ctx.ast;
        if self.addresses.len() <= 3 {
            let mut addresses = self.addresses.iter();
            let init = ast.eq_cmp(addr, ast.int_lit(*addresses.next().unwrap()));
            addresses.fold(init, |acc, e| {
                ast.and(acc, ast.eq_cmp(addr, ast.int_lit(*e)))
            })
        } else {
            let range = ast.and(
                ast.le_cmp(ast.int_lit(self.lower), addr),
                ast.lt_cmp(addr, ast.int_lit(self.upper)),
            );
            let offset = self.lower % self.stride;
            let stride = ast.eq_cmp(
                ast.module(addr, ast.int_lit(self.stride)),
                ast.int_lit(offset),
            );
            ast.and(range, stride)
        }
    }

    pub fn gen_boilerplate<'a>(
        &self,
        ctx: &mut ViperEncodeCtx<'a>,
        state: Vec<Expr>,
    ) -> Result<Vec<viper::Method<'a>>, ToViperError> {
        let ast = ctx.ast;
        let addr = ast.new_var("addr", ast.int_type());
        let retval = ast.new_var(ctx.return_var_name(), ast.int_type());
        let value = ast.new_var("value", ast.int_type());
        let mut methods = vec![];
        if self.typ.is_read() {
            let mut pres = state.clone().to_viper(ctx)?;
            let mut posts = pres.clone();
            pres.push(self.get_precondition(ctx, addr.1));
            posts.push(ctx.utils.bounded_f(retval.1, self.size.bits() as u64));
            methods.push(ast.method(
                &format!("load_{}", self.name),
                &[ctx.heap_var().0, ctx.state_var().0, addr.0],
                &[retval.0],
                &pres,
                &posts,
                Some(ast.seqn(&[ast.comment("TODO")], &[])),
            ));
        }
        if self.typ.is_write() {
            let mut pres = state.clone().to_viper(ctx)?;
            let mut posts = pres.clone();
            pres.push(self.get_precondition(ctx, addr.1));
            pres.push(ctx.utils.bounded_f(value.1, self.size.bits() as u64));
            methods.push(ast.method(
                &format!("store_{}", self.name),
                &[ctx.heap_var().0, ctx.state_var().0, addr.0, value.0],
                &[],
                &pres,
                &posts,
                Some(ast.seqn(&[ast.comment("TODO")], &[])),
            ));
        }
        Ok(methods)
    }
}

fn get_const(expr: &Expr) -> i64 {
    if let Expr::Const(i) = expr {
        return *i;
    }
    panic!(
        "Shared prototype address is not a constant expression: {}",
        expr
    )
}

impl SharedContext {
    pub fn new(options: &EncodeOptions, shared: &[Shared]) -> Self {
        let mut sctx = Self::default();
        for s in shared {
            sctx.add(&options, s);
        }
        sctx
    }

    fn get_idx(bits: usize) -> usize {
        match bits {
            8 => 0,
            16 => 1,
            32 => 2,
            64 => 3,
            _ => unreachable!(),
        }
    }

    fn add(&mut self, options: &EncodeOptions, shared: &Shared) {
        println!(
            "Registering shared memory functions ({}) for `{}`",
            shared.typ, shared.name,
        );
        let idx = Self::get_idx(shared.bits as usize);
        let lower = get_const(&shared.lower);
        let upper = get_const(&shared.upper);
        let stride = get_const(&shared.stride);
        let addresses = (lower..upper).step_by(stride as usize);

        for addr in addresses.clone() {
            for offset in 0..(shared.bits as i64 / 8) {
                if shared.typ.is_read()
                    && !self.read_addresses.insert(addr + offset)
                    && !options.ignore_warnings
                {
                    println!(
                        " - WARNING! Shared address {:#x} of {} is defined multiple times for reading",
                        addr + offset,
                        shared.name
                    );
                }
                if shared.typ.is_write()
                    && !self.write_addresses.insert(addr + offset)
                    && !options.ignore_warnings
                {
                    println!(
                        " - WARNING! Shared address {:#x} of {} is defined multiple times for writing",
                        addr + offset,
                        shared.name
                    );
                }
            }
        }

        self.mappings[idx].push(SharedInternal {
            name: shared.name.clone(),
            typ: shared.typ,
            size: shared.bits.into(),
            lower,
            upper,
            stride,
            addresses: addresses.collect(),
        });
    }

    pub fn get_method_name(
        &self,
        addr: i64,
        options: EncodeOptions,
        op: SharedOpType,
        size: MemOpBytes,
    ) -> String {
        let idx = Self::get_idx(size.bits() as usize);
        self.mappings[idx]
            .iter()
            .filter(|&s| s.typ.is_allowed(op))
            .find(|&s| s.addresses.iter().any(|a| *a == addr))
            .map(|si| format!("{}_{}", op, si.name))
            .unwrap_or_else(|| {
                if options.allow_undefined_shared {
                    format!("shared_{}{}", op, size.bits())
                } else {
                    panic!(
                        "No shared memory function registered for {} opearation at address {}",
                        op, addr
                    )
                }
            })
    }

    pub fn get_switch<'a>(
        &self,
        ctx: &ViperEncodeCtx<'a>,
        addr: viper::Expr<'a>,
        optyp: SharedOpType,
        bits: MemOpBytes,
        op2: viper::Expr<'a>,
    ) -> viper::Stmt<'a> {
        let ast = ctx.ast;
        let heap = ctx.heap_var().1;
        let state = ctx.state_var().1;
        let (args, rets) = match optyp {
            SharedOpType::Store => (vec![heap, state, addr, op2], vec![]),
            SharedOpType::Load => (vec![heap, state, addr], vec![op2]),
        };
        let init = if ctx.options.allow_undefined_shared {
            ast.method_call(&format!("shared_{}{}", optyp, bits.bits()), &args, &rets)
        } else {
            ast.assert(ast.false_lit(), ast.no_position())
        };
        self.mappings[Self::get_idx(bits.bits() as usize)]
            .iter()
            .filter(|&s| s.typ.is_allowed(optyp))
            .map(|s| (s, s.get_precondition(ctx, addr)))
            .fold(init, |acc, (s, cond)| {
                ast.if_stmt(
                    cond,
                    ast.seqn(
                        &[ast.method_call(&format!("{}_{}", optyp, s.name), &args, &rets)],
                        &[],
                    ),
                    ast.seqn(&[acc], &[]),
                )
            })
    }

    pub fn gen_boilerplate<'a>(
        &self,
        ctx: &mut ViperEncodeCtx<'a>,
        state: Vec<Expr>,
    ) -> Result<Vec<viper::Method<'a>>, ToViperError> {
        Ok(self
            .mappings
            .iter()
            .flatten()
            .map(|s| s.gen_boilerplate(ctx, state.clone()))
            .flatten()
            .flatten()
            .collect())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SharedOpType {
    Load,
    Store,
}

impl Display for SharedOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load => write!(f, "load"),
            Self::Store => write!(f, "store"),
        }
    }
}
