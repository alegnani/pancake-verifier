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
            Self::Word => 64,
        }
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
