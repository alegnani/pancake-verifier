use std::{cell::RefCell, fs::File, io::Write, rc::Rc};

use crate::{
    cli::{get_cake_path, get_viper_path, get_z3_path, CliOptions},
    ir, pancake,
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
    viper_path: String,
    cake_path: String,
    z3_path: String,
    pub model: Option<String>,
    output_path: Option<String>,
    pub verify: bool,
    viper: Option<Rc<RefCell<ViperHandle>>>,
}

impl Default for App {
    fn default() -> Self {
        Self {
            options: Default::default(),
            print: true,
            code: "".into(),
            viper_path: get_viper_path(),
            cake_path: get_cake_path(),
            z3_path: get_z3_path(),
            model: None,
            output_path: None,
            verify: true,
            viper: None,
        }
    }
}

impl App {
    pub fn new(options: CliOptions, print: bool) -> Self {
        let code = options.cmd.get_input();
        let viper_path = options.viper_path.clone();
        let cake_path = options.cake_path.clone();
        let z3_path = options.z3_exe.clone();
        let model = options.model.clone().map(|m| m.contents().unwrap());
        let output_path = options.cmd.get_output_path();
        let verify = options.cmd.get_verify();
        let options = options.into();
        Self {
            print,
            code,
            viper_path,
            cake_path,
            z3_path,
            options,
            model,
            output_path,
            verify,
            viper: None,
        }
    }

    pub fn set_viper(&mut self, viper: ViperHandle) {
        self.viper = Some(Rc::new(RefCell::new(viper)));
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

    pub fn verify_code(&self, viper: &mut ViperHandle, program: Program<'_>) -> Result<()> {
        self.println("Verifying...");
        let (s, success) = viper.verify(program);
        self.println(&s);
        if !success {
            return Err(anyhow!("Failed verification"));
        }
        Ok(())
    }

    pub fn run(&self) -> Result<()> {
        // Transpile the program
        let viper = self
            .viper
            .clone()
            .unwrap_or(Rc::new(RefCell::new(ViperHandle::new(
                self.viper_path.clone(),
                self.z3_path.clone(),
            ))));

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

        self.println("Transpiling to Viper...");
        let program = program.to_viper(ctx, viper.borrow().ast, self.options)?;

        let mut transpiled = viper.borrow().pretty_print(program);
        if let Some(mut model) = self.model.clone() {
            model.push_str("\n\n");
            model.push_str(&transpiled);
            transpiled = model;
        }
        if let Some(path) = &self.output_path {
            let mut file = File::create(path)
                .map_err(|e| anyhow!(format!("Error: Could not open output file:\n{}", e)))?;
            file.write_all(transpiled.as_bytes())
                .map_err(|e| anyhow!(format!("Error: Could not write to output file:\n{}", e)))?;
        }
        if self.verify {
            self.verify_code(&mut viper.borrow_mut(), program)?;
        }

        Ok(())
    }
}
