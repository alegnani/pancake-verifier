use std::env;

use ir_to_viper::EncodeOptions;
use lazy_static::lazy_static;

use super::*;

lazy_static! {
    static ref VIPER: Viper = Viper::new_with_args(&env::var("VIPER_HOME").unwrap(), vec![]);
}

fn verify_file(path: &str) -> anyhow::Result<()> {
    let cake = env::var("CAKE_ML").unwrap_or("cake".into());

    // Parse Pancake program
    let program: ir::Program = pancake::Program::parse_file(path, &cake)?.into();
    // Create Viper context
    // let viper = Viper::new_with_args(&viper_home, vec![]);
    let ver_ctx = VIPER.attach_current_thread();
    let ast_factory = ver_ctx.new_ast_factory();
    let mut verifier = ver_ctx.new_verifier_with_default_smt_and_extra_args(
        viper::VerificationBackend::Silicon,
        vec!["--logLevel=OFF".into()],
    );
    // Tranpile to Viper and verify
    let program = program.to_viper(ast_factory, EncodeOptions::default());
    let res = verifier.verify(program);
    assert!(res.is_success(), "Verification error: {:?}", res);

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
