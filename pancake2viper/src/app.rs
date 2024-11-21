use std::path::Path;
use std::process::Command;
use std::rc::Rc;
use std::{fs::File, io::Write};

use crate::utils::{MethodContext, ViperEncodeCtx};
use crate::{
    cli::{get_cake_path, get_viper_path, get_z3_path, CliOptions},
    ir::{self, shared::SharedContext},
    pancake,
    utils::{ConstEval, EncodeOptions, Mangleable, Mangler, ProgramToViper, ViperHandle},
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
    options: EncodeOptions,
    print: bool,
    pub code: String,
    cake_path: String,
    viper_path: String,
    z3_path: String,
    pub model: Option<String>,
    output_path: Option<String>,
    pub verify: bool,
    pub generate: bool,
}

impl Default for App {
    fn default() -> Self {
        Self {
            options: Default::default(),
            print: true,
            code: "".into(),
            cake_path: get_cake_path(),
            viper_path: get_viper_path(),
            z3_path: get_z3_path(),
            model: None,
            output_path: None,
            verify: true,
            generate: false,
        }
    }
}

impl App {
    pub fn new(options: CliOptions, print: bool) -> Self {
        let code = options.cmd.get_input();
        let cake_path = options.cake_path.clone();
        let viper_path = options.viper_path.clone();
        let z3_path = options.z3_exe.clone();
        let model = options.model.clone().map(|m| m.contents().unwrap());
        let output_path = options.cmd.get_output_path();
        let verify = options.cmd.get_verify();
        let generate = options.cmd.is_generate();
        let options = options.into();
        Self {
            print,
            code,
            cake_path,
            viper_path,
            z3_path,
            options,
            model,
            output_path,
            verify,
            generate,
        }
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

    pub fn verify_code(&self, verifier: &mut ViperHandle, program: Program<'_>) -> Result<()> {
        self.println("Verifying...");
        let (s, success) = verifier.verify(program);
        self.println(&s);
        if !success {
            return Err(anyhow!("Failed verification"));
        }
        Ok(())
    }

    pub fn run(&self, viper: &'static viper::Viper) -> Result<()> {
        let mut viper_handle = ViperHandle::from_handle(viper, self.z3_path.clone());
        let mut program: ir::Program = run_step!(self, "Parsing S-expr from cake", {
            pancake::Program::parse_str(self.code.clone(), &self.cake_path)
        })?
        .try_into()?;

        run_step!(self, "Mangling", {
            program.mangle(&mut Mangler::default())?
        });

        let ctx = run_step!(self, "Resolving types", { program.resolve_types()? });
        run_step!(self, "Evaluating constant expressions", {
            program = program.const_eval(&self.options)
        });

        if self.generate {
            self.println("Generating model boilerplate");
            let state = program.state.clone();
            let shared = Rc::new(SharedContext::new(&self.options, &program.shared));
            let method_ctx = Rc::new(MethodContext::new(&program.functions));

            let mut ctx = ViperEncodeCtx::new(
                ctx,
                program.predicates.iter().map(|p| p.name.clone()).collect(),
                viper_handle.ast,
                self.options,
                shared.clone(),
                method_ctx,
                state.clone(),
            );
            let gen_methods = shared.gen_boilerplate(&mut ctx, state)?;
            let program = viper_handle.ast.program(&[], &[], &[], &[], &gen_methods);
            let boilerplate = viper_handle.utils.pretty_print(program);
            let path = self.output_path.clone().unwrap();
            let mut file = File::create(path)
                .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
            file.write_all(boilerplate.as_bytes())
                .map_err(|e| anyhow!(format!("Error: Could not write to output file:\n{}", e)))?;
        } else {
            self.println("Transpiling to Viper...");
            let program = program.to_viper(ctx, viper_handle.ast, self.options)?;

            let mut transpiled = viper_handle.utils.pretty_print(program);
            if let Some(mut model) = self.model.clone() {
                model.push_str("\n\n");
                model.push_str(&transpiled);
                transpiled = model;
            }
            if let Some(path) = &self.output_path {
                let mut file = File::create(path)
                    .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
                file.write_all(transpiled.as_bytes()).map_err(|e| {
                    anyhow!(format!("Error: Could not write to output file:\n{}", e))
                })?;
            }
            if self.verify {
                if self.model.is_none() {
                    self.verify_code(&mut viper_handle, program)?;
                } else {
                    // When using a model we just add the model to the transpiled program and pass
                    // it to Viper via CLI
                    let mut file = File::create("tmp.vpr").map_err(|e| {
                        anyhow!(format!("Error: Could not create temporary file:\n{}", e))
                    })?;
                    file.write_all(transpiled.as_bytes()).map_err(|e| {
                        anyhow!(format!("Error: Could not write to temporary file:\n{}", e))
                    })?;
                    let path = Path::new(&self.viper_path).join("viperserver.jar");
                    // TODO: use jni crate
                    let verify = Command::new("java")
                        .args([
                            "-Xss30M",
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
                        return Err(anyhow!("Verification failure"));
                    }
                }
            }
        }
        Ok(())
    }
}
