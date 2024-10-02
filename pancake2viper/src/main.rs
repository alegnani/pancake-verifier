use anyhow::anyhow;
use clap::Parser;
use pancake2viper::{cli::CliOptions, ir, pancake, ProgramToViper};
use std::{fs::File, io::Write};

use viper::Viper;

fn main() -> anyhow::Result<()> {
    let options = CliOptions::parse();
    let encode_options = options.clone().into();

    let viper = Viper::new_with_args(&options.viper_path, vec![]);
    let ver_ctx = viper.attach_current_thread();
    let ast_factory = ver_ctx.new_ast_factory();
    let ast_utils = ver_ctx.new_ast_utils();
    let mut verifier = ver_ctx.new_verifier_with_default_smt_and_extra_args(
        viper::VerificationBackend::Silicon,
        vec!["--logLevel=OFF".into()],
    );

    let program_str = options.file.contents()?;
    let program: pancake::Program = pancake::Program::parse_str(program_str, &options.cake_path)?;
    let program: ir::Program = program.into();
    let program: viper::Program<'_> = program.to_viper(ast_factory, encode_options);

    let transpiled = ast_utils.pretty_print(program);
    if options.print_transpiled {
        println!("{}", transpiled);
    }
    if let Some(path) = options.output_path {
        let mut file = File::create(path)
            .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
        file.write_all(transpiled.as_bytes())
            .map_err(|e| anyhow!(format!("Error: Could not write to output file:\n{}", e)))?;
    }
    if options.verify {
        use viper::VerificationResult::*;
        match verifier.verify(program) {
            Success => println!("️✅Verification Successful✅"),
            Failure(e) => {
                let errors = e.into_iter().map(|e| e.message).collect::<Vec<_>>();
                println!("❌Verification Error❌\n\n{}", errors.join("\n\n"));
            }
            ConsistencyErrors(e) => println!("❌Consistency Error❌\n\n{}", e.join("\n\n")),
            JavaException(e) => println!("❌Java Exception❌\n\n{}", e),
        };
    }

    Ok(())
}
