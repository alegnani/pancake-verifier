pub mod bitvector;
pub mod ext_calls;
pub mod iarray;
pub mod shared_mem;
pub mod utils;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use shared_mem::create_shared_mem_methods;
use utils::{bound_function, pow_function};
use viper::{AstFactory, Domain, Field, Function, Method};

use crate::utils::EncodeOptions;

pub fn create_viper_prelude(
    ast: AstFactory,
    options: EncodeOptions,
) -> (Vec<Domain>, Vec<Field>, Vec<Method>, Vec<Function>) {
    if !options.include_prelude {
        return (vec![], vec![], vec![], vec![]);
    }
    let iarray = IArrayHelper::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    let fields = vec![iarray.field()];
    let mut methods = iarray.slice_defs();
    methods.extend(create_shared_mem_methods(ast));
    (
        domains,
        fields,
        methods,
        vec![pow_function(ast), bound_function(ast, options.word_size)],
    )
}
