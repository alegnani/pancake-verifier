pub mod bitvector;
pub mod ext_calls;
pub mod iarray;
pub mod shared_mem;

use bitvector::create_bv_domain;
pub use iarray::IArrayHelper;
use shared_mem::create_shared_mem_methods;
use viper::{AstFactory, Domain, Field, Function, Method};

use crate::utils::ViperUtils;

fn pow_function(ast: AstFactory) -> Function {
    let (e_decl, e) = ast.new_var("e", ast.int_type());
    let body = ast.cond_exp(
        ast.eq_cmp(e, ast.zero()),
        ast.one(),
        ast.mul(
            ast.two(),
            ast.func_app(
                "pow",
                &[ast.sub(e, ast.one())],
                ast.int_type(),
                ast.no_position(),
            ),
        ),
    );
    ast.function(
        "pow",
        &[e_decl],
        ast.int_type(),
        &[ast.ge_cmp(e, ast.zero())],
        &[],
        ast.no_position(),
        Some(body),
    )
}

pub fn create_viper_prelude(
    ast: AstFactory,
) -> (Vec<Domain>, Vec<Field>, Vec<Method>, Vec<Function>) {
    let iarray = IArrayHelper::new(ast);
    let domains = vec![iarray.domain, create_bv_domain(ast)];
    let fields = vec![iarray.field()];
    let mut methods = iarray.slice_defs();
    methods.extend(create_shared_mem_methods(ast));
    (domains, fields, methods, vec![pow_function(ast)])
}
