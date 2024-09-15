pub mod bitvector;
pub mod iarray;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use viper::{AstFactory, Domain, Field, Method};

pub fn create_viper_prelude(ast: AstFactory) -> (Vec<Domain>, Vec<Field>, Vec<Method>) {
    let iarray = IArrayHelper::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    let fields = vec![iarray.field()];
    let methods = iarray.slice_defs();
    (domains, fields, methods)
}
