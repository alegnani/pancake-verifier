use std::ops::Add;

use crate::utils::{Shape, ToType};

use super::{
    expression::{Expr, Struct},
    statement::MemOpBytes,
    Arg, BinOp, BinOpType, Decl, Type, UnOpType,
};

impl Struct {
    pub fn new(elements: Vec<Expr>) -> Self {
        Self { elements }
    }

    pub fn flatten(&self) -> Vec<Expr> {
        self.elements
            .iter()
            .flat_map(|e| match e {
                Expr::Struct(inner) => inner.flatten(),
                x => vec![x.clone()],
            })
            .collect()
    }
}

impl MemOpBytes {
    pub fn bits(&self) -> u32 {
        match self {
            Self::Byte => 8,
            Self::HalfWord => 32,
            Self::Word => 64,
        }
    }

    pub fn bytes(&self) -> u32 {
        self.bits() / 8
    }
}

impl From<u64> for MemOpBytes {
    fn from(value: u64) -> Self {
        match value {
            8 => Self::Byte,
            32 => Self::HalfWord,
            64 => Self::Word,
            x => panic!("invalid conversion from u64 to MemOpBytes, got {}", x),
        }
    }
}

impl From<Decl> for Arg {
    fn from(value: Decl) -> Self {
        Self {
            name: value.name,
            typ: value.typ,
        }
    }
}

impl Type {
    pub fn len(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Int | Type::Bool => 1,
            Type::Struct(inner) => inner.iter().map(Shape::len).sum(),
            _ => panic!("Unbounded length"),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ToType for BinOpType {
    fn to_type(&self) -> super::Type {
        use BinOpType::*;
        match self {
            Gt | Gte | Lt | Lte | BoolAnd | BoolOr | ViperEqual | ViperNotEqual => Type::Bool,
            PancakeEqual | PancakeNotEqual => Type::Bool, // FIXME: enforce `===` vs `==`
            _ => Type::Int,
        }
    }
}

impl ToType for UnOpType {
    fn to_type(&self) -> super::Type {
        match self {
            UnOpType::Minus => Type::Int,
            UnOpType::Neg => Type::Bool,
        }
    }
}

impl Add<i64> for Expr {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        Expr::BinOp(BinOp {
            optype: BinOpType::Add,
            left: Box::new(self),
            right: Box::new(Expr::Const(rhs)),
        })
    }
}
