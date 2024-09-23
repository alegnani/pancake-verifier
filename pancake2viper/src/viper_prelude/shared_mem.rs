use viper::{AstFactory, Method};

use crate::utils::ViperUtils;

pub fn create_shared_mem_methods(ast: AstFactory<'_>) -> Vec<Method<'_>> {
    let address = ast.new_var("address", ast.int_type());
    let value = ast.new_var("value", ast.int_type());
    let store = ast.method("shared_store", &[address.0, value.0], &[], &[], &[], None);
    let load = ast.method("shared_load", &[address.0], &[value.0], &[], &[], None);
    vec![load, store]
}
