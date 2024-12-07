use viper::{AstFactory, Method};

use crate::utils::ViperUtils;

use super::utils::Utils;

pub fn create_shared_mem_methods<'a>(ast: AstFactory<'a>, utils: &Utils<'a>) -> Vec<Method<'a>> {
    let address = ast.new_var("address", ast.int_type());
    let heap = utils.heap();
    let value = ast.new_var("value", ast.int_type());
    let mut store_args = utils.get_model().get_default_args(ast, heap).0;
    store_args.push(address.0);
    let load_args = store_args.clone();
    store_args.push(value.0);

    [8, 16, 32, 64]
        .into_iter()
        .flat_map(|bits| {
            let store = ast.method(
                &format!("shared_store{}", bits),
                &store_args,
                &[],
                &[utils.bounded_f(value.1, bits)],
                &[],
                None,
            );
            let load = ast.method(
                &format!("shared_load{}", bits),
                &load_args,
                &[value.0],
                &[],
                &[utils.bounded_f(value.1, bits)],
                None,
            );
            vec![load, store]
        })
        .collect()
}
