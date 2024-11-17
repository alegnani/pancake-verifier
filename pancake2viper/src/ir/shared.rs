use super::{Expr, MemOpBytes, Shared};
use std::collections::HashSet;

#[derive(Debug, Clone, Default)]
pub struct SharedContext {
    addresses: HashSet<u64>,
    mappings: [Vec<SharedInternal>; 4],
}

#[derive(Debug, Clone)]
struct SharedInternal {
    name: String,
    size: MemOpBytes,
    addresses: Vec<u64>,
    lower: u64,
    upper: u64,
    stride: u64,
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
    pub fn new(shared: &[Shared]) -> Self {
        let mut se = Self::default();
        for s in shared {
            se.add(s);
        }
        se
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

    pub fn add(&mut self, shared: &Shared) {
        println!(
            "Registering shared memory functions {}_store and {}_load",
            shared.name, shared.name
        );
        let idx = Self::get_idx(shared.bits as usize);
        let lower = get_const(&shared.lower) as u64;
        let upper = get_const(&shared.upper) as u64;
        let stride = get_const(&shared.stride) as u64;
        let addresses = (lower..upper).step_by(stride as usize);
        for addr in addresses.clone() {
            for offset in 0..(shared.bits / 8) {
                if !self.addresses.insert(addr + offset) {
                    println!(
                        " - WARNING! Shared address {:#x} of {} is defined multiple times",
                        addr + offset,
                        shared.name
                    );
                }
            }
        }

        self.mappings[idx].push(SharedInternal {
            name: shared.name.clone(),
            size: shared.bits.into(),
            lower: lower as u64,
            upper: stride as u64,
            stride: stride as u64,
            addresses: addresses.collect(),
        });
    }

    pub fn get_method_name<'a>(&self, addr: u64, size: MemOpBytes) -> String {
        let idx = Self::get_idx(size.bits() as usize);
        self.mappings[idx]
            .iter()
            .find(|&s| s.addresses.iter().any(|a| *a == addr))
            .unwrap_or_else(|| panic!("No shared memory function registered for address {}", addr))
            .name
            .clone()
    }
}
