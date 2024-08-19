use std::env;

use viper::{utils, AstUtils, VerificationContext, Verifier, Viper};

use crate::{
    pancake,
    translation::{top::ViperEncodeCtx, ToViper},
};

pub struct PancakeVerifier<'a> {
    viper: Viper,
    vctx: VerificationContext<'a>,
    verifier: Verifier<'a>,
    utils: AstUtils<'a>,
    pancake_program: pancake::Program,
    viper_program: viper::Program<'a>,
    trans_ctx: ViperEncodeCtx<'a>,
}

pub fn new(program: pancake::Program) -> anyhow::Result<()> {
    let viper_home = env::var("VIPER_HOME")?;
    let viper = Viper::new(&viper_home);
    let vctx = viper.attach_current_thread();
    let ast = vctx.new_ast_factory();
    // let verifier = vctx
    //     .new_verifier_with_default_smt_and_extra_args(viper::VerificationBackend::Silicon, vec![]);
    // let utils = vctx.new_ast_utils();
    // let trans_ctx = ViperEncodeCtx::new(ast);

    Ok(())
}

pub fn pretty_print(program: pancake::Program) -> anyhow::Result<String> {
    let viper_home = env::var("VIPER_HOME")?;
    let viper = Viper::new(&viper_home);
    let vctx = viper.attach_current_thread();
    let utils = vctx.new_ast_utils();
    let ast = vctx.new_ast_factory();
    let mut ctx = ViperEncodeCtx::new(ast);
    Ok(utils.pretty_print(program.to_viper(&mut ctx)))
}
