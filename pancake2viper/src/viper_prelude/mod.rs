pub mod bitvector;
pub mod iarray;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use viper::{AstFactory, Domain, Field, Method};

pub fn create_viper_prelude(ast: AstFactory) -> (Vec<Domain>, Vec<Field>, Vec<Method>) {
    let iarray = IArrayHelper::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    // TODO: split fields into the functions
    let fields = vec![iarray.field()];
    let methods = vec![iarray.create_slice_def()];
    (domains, fields, methods)
}
