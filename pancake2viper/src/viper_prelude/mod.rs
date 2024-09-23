pub mod bitvector;
pub mod iarray;
pub mod shared_mem;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use shared_mem::create_shared_mem_methods;
use viper::{AstFactory, Domain, Field, Method};

pub fn create_viper_prelude(ast: AstFactory) -> (Vec<Domain>, Vec<Field>, Vec<Method>) {
    let iarray = IArrayHelper::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    let fields = vec![iarray.field()];
    let mut methods = iarray.slice_defs();
    methods.extend(create_shared_mem_methods(ast));
    (domains, fields, methods)
}
