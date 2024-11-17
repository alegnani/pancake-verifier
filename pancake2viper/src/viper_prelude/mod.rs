pub mod bitvector;
pub mod ext_calls;
pub mod iarray;
pub mod shared_mem;
pub mod utils;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use shared_mem::create_shared_mem_methods;
use utils::{bound_bits_function, bound_function, Utils};
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
    let utils = Utils::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    let fields = vec![iarray.field()];
    let mut methods = iarray.slice_defs();
    methods.extend(create_shared_mem_methods(ast, &utils));
    (
        domains,
        fields,
        methods,
        [8, 16, 32, 64]
            .into_iter()
            .map(|bits| bound_bits_function(ast, bits))
            .chain(std::iter::once(bound_function(ast, &utils, options)))
            .collect(),
    )
}
