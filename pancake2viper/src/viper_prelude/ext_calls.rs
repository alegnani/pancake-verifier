use crate::utils::ViperUtils;
use viper::{AstFactory, Method};

pub fn add_ext_call<'a>(ast: AstFactory<'a>, name: &str) -> Method<'a> {
    let args = (0..4)
        .map(|i| ast.new_var(&format!("ffi_arg_{}", i), ast.int_type()).0)
        .collect::<Vec<_>>();
    ast.method(name, &args, &[], &[], &[], None)
}
