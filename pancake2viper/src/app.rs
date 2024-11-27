use std::collections::HashSet;
use std::path::Path;
use std::process::Command;
use std::rc::Rc;
use std::time::Instant;
use std::{fs::File, io::Write};

use crate::cli::{self, CliOptions};
use crate::utils::{EncodeOptions, MethodContext, TypeContext, ViperEncodeCtx};
use crate::{
    ir::{self, shared::SharedContext},
    pancake,
    utils::{ConstEval, Mangleable, Mangler, ProgramToViper, ViperHandle},
};
use anyhow::{anyhow, Result};
use viper::Program;

macro_rules! run_step {
    ($self:ident, $name:literal, $stmts:block) => {{
        $self.print(&format!("{}...", $name));
        let ret = $stmts;
        $self.println("DONE");
        ret
    }};
}

pub struct App {
    pub options: CliOptions,
    pub print: bool,
}

impl Default for App {
    fn default() -> Self {
        Self {
            options: CliOptions::default(),
            print: true,
        }
    }
}

impl App {
    pub fn new_verification(input: String, print: bool) -> Self {
        let options = cli::CliOptions {
            cmd: cli::Command::Verify(cli::Verify { input }),
            ..Default::default()
        };
        Self { options, print }
    }

    pub fn new(options: CliOptions, print: bool) -> Self {
        Self { options, print }
    }

    fn print(&self, s: &str) {
        if self.print {
            print!("{}", s)
        }
    }

    fn println(&self, s: &str) {
        if self.print {
            println!("{}", s)
        }
    }

    fn verify(
        &self,
        verifier: &mut ViperHandle,
        program: Program<'_>,
        transpiled: String,
        exclude_list: &[String],
        use_viper_cli: bool,
    ) -> Result<()> {
        if use_viper_cli {
            self.verify_code_model(transpiled, exclude_list)
        } else {
            self.verify_code_no_model(verifier, program)
        }
    }

    fn verify_code_no_model(&self, verifier: &mut ViperHandle, program: Program<'_>) -> Result<()> {
        self.println("Verifying...");
        let (s, success) = verifier.verify(program);
        self.println(&s);
        if !success {
            return Err(anyhow!("Failed verification"));
        }
        Ok(())
    }

    fn verify_code_model(&self, transpiled: String, exclude_list: &[String]) -> Result<()> {
        // When using a model we just add the model to the transpiled program and pass
        // it to Viper via CLI
        let mut file = File::create("tmp.vpr")
            .map_err(|e| anyhow!(format!("Error: Could not create temporary file:\n{}", e)))?;
        file.write_all(transpiled.as_bytes())
            .map_err(|e| anyhow!(format!("Error: Could not write to temporary file:\n{}", e)))?;
        let path = Path::new(&self.options.viper_path).join("viperserver.jar");

        let verify = Command::new("java")
            .args([
                "-Xss300M",
                "-cp",
                path.to_str().unwrap(),
                "viper.silicon.SiliconRunner",
                "--logLevel=OFF",
                "--exhaleMode=1",
                "tmp.vpr",
            ])
            .spawn()?
            .wait_with_output()?;
        if !verify.status.success() {
            Err(anyhow!("Verification failure"))
        } else {
            Ok(())
        }
    }

    pub fn generate(
        &self,
        type_ctx: TypeContext,
        viper_handle: &ViperHandle,
        encode_opts: EncodeOptions,
        program: ir::Program,
        output_path: String,
    ) -> Result<()> {
        self.println("Generating model boilerplate");
        let state = program.state.clone();
        let shared = Rc::new(SharedContext::new(&encode_opts, &program.shared));
        let method_ctx = Rc::new(MethodContext::new(&program.functions));

        let mut ctx = ViperEncodeCtx::new(
            type_ctx,
            program.predicates.iter().map(|p| p.name.clone()).collect(),
            viper_handle.ast,
            encode_opts,
            shared.clone(),
            method_ctx,
            state.clone(),
        );
        let gen_methods = shared.gen_boilerplate(&mut ctx, state)?;
        let program = viper_handle.ast.program(&[], &[], &[], &[], &gen_methods);
        let boilerplate = viper_handle.utils.pretty_print(program);

        let mut file = File::create(output_path)
            .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
        file.write_all(boilerplate.as_bytes())
            .map_err(|e| anyhow!(format!("Error: Could not write to output file:\n{}", e)))?;
        Ok(())
    }

    pub fn run(&self, viper: &'static viper::Viper) -> Result<()> {
        let use_viper_cli = self.options.model.is_some() || !self.options.include.is_empty();
        let mut viper_handle = ViperHandle::from_handle(viper, self.options.z3_exe.clone());
        let mut program: ir::Program = run_step!(self, "Parsing S-expr from cake", {
            pancake::Program::parse_str(self.options.cmd.get_input(), &self.options.cake_path)
        })?
        .try_into()?;
        let encode_opts = self.options.clone().into();

        run_step!(self, "Mangling", {
            program.mangle(&mut Mangler::default())?
        });

        let ctx = run_step!(self, "Resolving types", { program.resolve_types()? });
        run_step!(self, "Evaluating constant expressions", {
            program = program.const_eval(&encode_opts);
        });

        if let cli::Command::Generate(cli::Generate { output_path, .. }) = &self.options.cmd {
            return self.generate(
                ctx,
                &viper_handle,
                encode_opts,
                program,
                output_path.clone(),
            );
        }

        if let Some(only) = &self.options.only {
            program.trust_except(only);
        }

        self.println("Transpiling to Viper...");
        let vpr_program = program
            .clone()
            .to_viper(ctx.clone(), viper_handle.ast, encode_opts)?;
        let mut transpiled = viper_handle.utils.pretty_print(vpr_program);

        // Add includes
        let mut includes = self
            .options
            .include
            .iter()
            .map(std::fs::read_to_string)
            .collect::<Result<Vec<_>, _>>()?
            .join("\n\n");
        includes.push_str("\n\n");
        includes.push_str(&transpiled);
        transpiled = includes;

        // Add the model, if present
        if let Some(mut model) = self.options.model.clone() {
            model.push_str("\n\n");
            model.push_str(&transpiled);
            transpiled = model;
        }

        // Save the transpiled Viper code in a file
        if let Some(path) = &self.options.cmd.get_output_path() {
            let mut file = File::create(path)
                .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
            file.write_all(transpiled.as_bytes())
                .map_err(|e| anyhow!(format!("Error: Could not write to output file:\n{}", e)))?;
        }

        // Verify the Viper code
        if self.options.cmd.is_verify() {
            if self.options.incremental {
                let start = Instant::now();
                // check shared methods
                // TODO:

                // check transpiled Pancake functions
                for only in program.get_method_names().0 {
                    println!("\n========== Verifying function '{}' ==========\n", only);
                    let mut only_program = program.clone();
                    only_program.trust_except(&[only]);
                    let only_vpr =
                        only_program.to_viper(ctx.clone(), viper_handle.ast, encode_opts)?;
                    let mut new_transpiled = viper_handle.utils.pretty_print(only_vpr);
                    if let Some(mut model) = self.options.model.clone() {
                        model.push_str("\n\n");
                        model.push_str(&new_transpiled);
                        new_transpiled = model;
                    }

                    self.verify(
                        &mut viper_handle,
                        only_vpr,
                        new_transpiled,
                        &[],
                        use_viper_cli,
                    )?;
                }
                println!(
                    "Total verification time: {:.2}s",
                    start.elapsed().as_secs_f32()
                );
            } else {
                self.verify(
                    &mut viper_handle,
                    vpr_program,
                    transpiled,
                    &[],
                    use_viper_cli,
                )?;
            }
        }
        Ok(())
    }
}
