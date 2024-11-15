use anyhow::anyhow;
use clap::Parser;
use pancake2viper::{
    cli::CliOptions,
    ir, pancake,
    utils::{ConstEval, Mangleable, Mangler, ProgramToViper, ViperHandle},
};
use std::{fs::File, io::Write};

fn main() -> anyhow::Result<()> {
    let options = CliOptions::parse();
    let encode_options = options.clone().into();

    let mut viper = ViperHandle::new(options.viper_path.clone(), options.z3_exe.clone());

    print!("Parsing S-expr from cake...");
    let program_str = options.file.contents()?;
    let program: pancake::Program = pancake::Program::parse_str(program_str, &options.cake_path)?;
    println!("DONE");
    let mut program: ir::Program = program.try_into()?;
    print!("Mangling...");
    let mut mangler = Mangler::default();
    program.mangle(&mut mangler)?;
    println!("DONE");
    print!("Resolving types...");
    let ctx = program.resolve_types()?;
    println!("DONE");
    print!("Evaluating constant expressions...");
    let program = program.const_eval(&encode_options);
    println!("DONE");
    print!("Transpiling to Viper...");
    let program: viper::Program<'_> = program.to_viper(ctx, viper.ast, encode_options)?;
    println!("DONE");

    let transpiled = viper.pretty_print(program);
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
        println!("Verifying...");
        println!("{}", viper.verify(program));
    }

    Ok(())
}
