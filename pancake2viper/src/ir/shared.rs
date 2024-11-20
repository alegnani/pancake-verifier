use crate::utils::{EncodeOptions, ViperEncodeCtx};

use super::{Expr, MemOpBytes, Shared};
use std::{collections::HashSet, fmt::Display, option};

#[derive(Clone)]
pub struct SharedContext {
    addresses: HashSet<i64>,
    mappings: [Vec<SharedInternal>; 4],
}

#[derive(Debug, Clone)]
struct SharedInternal {
    name: String,
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
        let mut mappings = [vec![], vec![], vec![], vec![]];
        let mut addresses = HashSet::new();
        for s in shared {
            Self::add(&options, &mut mappings, &mut addresses, s);
        }
        Self {
            addresses,
            mappings,
        }
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

    fn add(
        options: &EncodeOptions,
        mappings: &mut [Vec<SharedInternal>; 4],
        addresses_set: &mut HashSet<i64>,
        shared: &Shared,
    ) {
        println!(
            "Registering shared memory functions {}_store and {}_load",
            shared.name, shared.name
        );
        let idx = Self::get_idx(shared.bits as usize);
        let lower = get_const(&shared.lower);
        let upper = get_const(&shared.upper);
        let stride = get_const(&shared.stride);
        let addresses = (lower..upper).step_by(stride as usize);
        for addr in addresses.clone() {
            for offset in 0..(shared.bits as i64 / 8) {
                if !addresses_set.insert(addr + offset) && !options.ignore_warnings {
                    println!(
                        " - WARNING! Shared address {:#x} of {} is defined multiple times",
                        addr + offset,
                        shared.name
                    );
                }
            }
        }

        mappings[idx].push(SharedInternal {
            name: shared.name.clone(),
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
            .find(|&s| s.addresses.iter().any(|a| *a == addr))
            .map(|si| format!("{}_{}", op, si.name))
            .unwrap_or_else(|| {
                if options.allow_undefined_shared {
                    format!("shared_{}{}", op, size.bits())
                } else {
                    panic!("No shared memory function registered for address {}", addr)
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
