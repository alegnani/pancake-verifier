use crate::utils::TranslationError;

use super::{Expr, Function, Method, Predicate, Struct};

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

impl Predicate {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Function {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Method {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Expr {
    // TODO: this could well be a function pointer. If we stick to only using
    // valid function addresses (no unholy pointer arithmetic) we can encode
    // this as a switch statement checking the expression against all possible
    // function addresses.
    pub fn get_label(&self) -> Result<String, TranslationError> {
        match self {
            Self::Label(label) => Ok(label.to_owned()),
            x => Err(TranslationError::InvalidLabel(x.clone())),
        }
    }
}
