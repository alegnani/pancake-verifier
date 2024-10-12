use viper::{AstFactory, Method};

use crate::utils::ViperUtils;

pub fn create_shared_mem_methods(ast: AstFactory<'_>) -> Vec<Method<'_>> {
    let address = ast.new_var("address", ast.int_type());
    let value = ast.new_var("value", ast.int_type());
    let upper = ast.new_var("upper", ast.int_type());
    let store = ast.method("shared_store", &[address.0, value.0], &[], &[], &[], None);

    let post = ast.and(
        ast.le_cmp(ast.int_lit(0), value.1),
        ast.lt_cmp(value.1, upper.1),
    );
    let load = ast.method(
        "shared_load",
        &[address.0, upper.0],
        &[value.0],
        &[],
        &[post],
        None,
    );
    vec![load, store]
}
