mod parser;
#[cfg(test)]
mod tests;

pub use parser::{parse_annot, parse_function, parse_method, parse_predicate};
