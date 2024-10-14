use crate::utils::{Shape, ToShape, TypeContext};

use super::{
    expression::{Expr, Struct},
    statement::MemOpBytes,
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

// impl Program {
//     pub fn infer_function_types(&self) -> HashMap<String, Shape> {
//         todo!()
//     }
// }

// impl FnDec {
//     fn get_type_ctx(&self) -> TypeContext {
//         todo!()
//     }
// }

// #[derive(Debug)]
// enum ExprType {
//     Symbolic(String),
//     Resolved(Shape),
// }

// #[derive(Debug, Default)]
// struct TypeContext {
//     var_map: HashMap<String, ExprType>,
// }
