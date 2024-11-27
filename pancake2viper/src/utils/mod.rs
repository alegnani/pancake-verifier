mod contexts;
mod errors;
mod mangler;
mod misc;
mod shape;
mod traits;

use std::collections::HashMap;

pub use contexts::*;
pub use errors::*;
pub use mangler::{Mangler, VariableType};
pub use misc::{ViperHandle, ViperUtils};
pub use shape::Shape;
pub use traits::*;

use crate::ir::types::Type;

lazy_static::lazy_static! {
    pub static ref RESERVED: HashMap<&'static str, Type> = HashMap::from([
        ("heap", Type::Array),
        ("read", Type::Void),
        ("write", Type::Void),
        ("wildcard", Type::Void),
        ("acc", Type::Bool),
        ("alen", Type::Int),
        ("old", Type::Wildcard),
        ("result", Type::Wildcard),
        ("state", Type::Ref),
        ("bounded", Type::Bool),
        ("bounded8", Type::Bool),
        ("bounded16", Type::Bool),
        ("bounded32", Type::Bool),
        ("bounded64", Type::Bool),
    ]);
}
