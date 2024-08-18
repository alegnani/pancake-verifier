use pancake2viper::{
    parser::{get_sexprs_from_file, SExprParser},
    translation::{ToViper, ViperEncodeCtx},
};
use std::env;

use viper::Viper;

fn main() -> anyhow::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    let sexprs = get_sexprs_from_file(&args[1], "cake")?;
    let program = SExprParser::parse_program(sexprs)?;

    // println!("\n\n################# VIPER #################\n\n");
    // let vpr_prelude = fs::read_to_string("src/prelude.vpr")?;
    // println!("{}\n", vpr_prelude);
    // for ast in asts {
    //     println!("{}", translate_fndec(&ast));
    // }

    let viper_home = env::var("VIPER_HOME")?;
    let viper = Viper::new(&viper_home);
    let ver_ctx = viper.attach_current_thread();
    let ast_factory = ver_ctx.new_ast_factory();
    let ast_utils = ver_ctx.new_ast_utils();
    let mut ctx = ViperEncodeCtx::new(ast_factory);
    let program = program.to_viper(&mut ctx);
    let mut verifier = ver_ctx
        .new_verifier_with_default_smt_and_extra_args(viper::VerificationBackend::Silicon, vec![]);
    let s = ast_utils.pretty_print(program);
    println!("{}", s);

    let res = verifier.verify(program);
    println!("Res: {:?}", res);

    Ok(())
}
