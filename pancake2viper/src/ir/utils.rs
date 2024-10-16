use crate::utils::Shape;

use super::{
    expression::{Expr, Struct},
    statement::MemOpBytes,
    Arg, Decl, Type,
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
        }
    }
}

impl From<Type> for Shape {
    fn from(value: Type) -> Self {
        match value {
            Type::Bool | Type::Int => Shape::Simple,
            Type::IArray => Shape::Nested(vec![]),
        }
    }
}

impl From<Decl> for Arg {
    fn from(value: Decl) -> Self {
        Self {
            name: value.name,
            shape: value.typ.into(),
        }
    }
}
