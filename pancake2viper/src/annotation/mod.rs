mod parser;
#[cfg(test)]
mod tests;

pub use parser::{
    parse_annot, parse_extern_ffi, parse_extern_field, parse_extern_predicate, parse_function,
    parse_method, parse_model_field, parse_model_predicate, parse_predicate, parse_shared,
};
