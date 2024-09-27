use pancake2viper::{
    ir::Program,
    parser::{get_sexprs_from_file, SExprParser},
    translation::{context::EncodeOptions, ProgramToViper},
};
use std::env;

use viper::Viper;

fn main() -> anyhow::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    // either use $CAKE_ML or search for cake on the PATH
    let cake = env::var("CAKE_ML").unwrap_or("cake".into());
    let sexprs = get_sexprs_from_file(&args[1], &cake)?;
    let program: Program = SExprParser::parse_program(sexprs)?.into();

    let viper_home = env::var("VIPER_HOME")?;
    let viper = Viper::new_with_args(&viper_home, vec![]);
    let ver_ctx = viper.attach_current_thread();
    let ast_factory = ver_ctx.new_ast_factory();
    let ast_utils = ver_ctx.new_ast_utils();
    let program = program.to_viper(ast_factory, EncodeOptions::default());
    let mut verifier = ver_ctx.new_verifier_with_default_smt_and_extra_args(
        viper::VerificationBackend::Silicon,
        vec!["--logLevel=OFF".into()],
    );
    let defines = "\
        define full_access(a, pm) forall j: Int :: 0 <= j < len(a) ==> acc(slot(a, j).heap_elem, pm)\n\
        define slice_access(a, idx, length, pm) forall j: Int :: 0 <= idx <= j < idx + length <= len(a) ==> acc(slot(a, j).heap_elem, pm)
    ";
    println!("// Defines\n{}", defines);
    let s = ast_utils.pretty_print(program);
    println!("{}", s);

    // let res = verifier.verify(program);
    // println!("Res: {:?}", res);

    Ok(())
}
