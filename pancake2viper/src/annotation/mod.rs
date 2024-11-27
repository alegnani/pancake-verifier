mod parser;
#[cfg(test)]
mod tests;

pub use parser::{
    parse_annot, parse_extern_field, parse_extern_predicate, parse_function, parse_method,
    parse_predicate, parse_shared, parse_state,
};
