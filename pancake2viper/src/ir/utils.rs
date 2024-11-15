use std::ops::Add;

use crate::utils::{Shape, ToType};

use super::{
    expression::{Expr, Struct},
    statement::MemOpBytes,
    Arg, BinOp, BinOpType, Decl, ShiftType, Type, UnOpType,
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
            Self::QuarterWord => 16,
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
            16 => Self::QuarterWord,
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

impl UnOpType {
    pub fn eval(&self, value: i64) -> i64 {
        match self {
            Self::Minus => -value,
            Self::Neg => {
                if value == 0 {
                    1
                } else {
                    0
                }
            }
        }
    }
}

impl BinOpType {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::Add | Self::Sub | Self::Div | Self::Mul | Self::Modulo
        )
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(self, Self::BitAnd | Self::BitOr | Self::BitXor)
    }

    pub fn eval(&self, lhs: i64, rhs: i64) -> i64 {
        match self {
            Self::Add => lhs + rhs,
            Self::BitAnd => lhs & rhs,
            Self::BitOr => lhs | rhs,
            Self::BitXor => lhs ^ rhs,
            Self::Div => lhs / rhs,
            Self::Modulo => lhs % rhs,
            Self::Mul => lhs * rhs,
            Self::Sub => lhs - rhs,
            x => {
                let b = match x {
                    Self::BoolAnd => (lhs != 0) && (rhs != 0),
                    Self::BoolOr => (lhs != 0) || (rhs != 0),
                    Self::Gt => lhs > rhs,
                    Self::Gte => lhs >= rhs,
                    Self::Lt => lhs < rhs,
                    Self::Lte => lhs <= rhs,
                    Self::Iff => (lhs != 0) == (rhs != 0),
                    Self::Imp => (lhs == 0) || (rhs != 0),
                    Self::PancakeEqual | Self::ViperEqual => lhs == rhs,
                    Self::PancakeNotEqual | Self::ViperNotEqual => lhs != rhs,
                    _ => unreachable!(),
                };
                if b {
                    1
                } else {
                    0
                }
            }
        }
    }
}

impl ShiftType {
    pub fn eval(&self, lhs: i64, rhs: u64) -> i64 {
        match self {
            ShiftType::Asr => lhs >> rhs,
            ShiftType::Lsl => lhs << rhs,
            ShiftType::Lsr => ((lhs as u64) >> rhs) as i64,
        }
    }
}
