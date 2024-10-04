use std::{path::PathBuf, str::FromStr};

use anyhow::anyhow;
use regex::Regex;
use viper::{
    smt_manager::SmtManager, AstFactory, AstUtils, Expr, LocalVarDecl, Type, VerificationContext,
    Verifier, Viper,
};

pub struct ViperHandle {
    pub viper: &'static Viper,
    pub ver_ctx: &'static VerificationContext<'static>,
    pub ast: AstFactory<'static>,
    pub utils: AstUtils<'static>,
    pub verifier: Verifier<'static>,
}

unsafe impl Sync for ViperHandle {}
unsafe impl Send for ViperHandle {}

impl ViperHandle {
    pub fn new(viper_home: String, z3_exe: String) -> Self {
        let viper = Box::new(Viper::new_with_args(&viper_home, vec![]));
        let viper = Box::leak(viper);
        let ver_ctx = viper.attach_current_thread();
        let ver_ctx = Box::leak(Box::new(ver_ctx));
        let ast = ver_ctx.new_ast_factory();
        let utils = ver_ctx.new_ast_utils();
        let verifier = ver_ctx.new_verifier(
            viper::VerificationBackend::Silicon,
            vec!["--logLevel=OFF".into()],
            None,
            z3_exe,
            None,
            SmtManager::default(),
        );
        // let verifier = ver_ctx.new_verifier_with_default_smt_and_extra_args(
        //     viper::VerificationBackend::Silicon,
        //     vec!["--logLevel=OFF".into()],
        // );
        Self {
            viper,
            ver_ctx,
            ast,
            utils,
            verifier,
        }
    }
}

impl ViperHandle {
    pub fn verify(&mut self, program: viper::Program) -> String {
        use viper::VerificationResult::*;
        match self.verifier.verify(program) {
            Success => "️✅Verification Successful✅".into(),
            Failure(e) => {
                let errors = e.into_iter().map(|e| e.message).collect::<Vec<_>>();
                format!("❌Verification Error❌\n\n{}", errors.join("\n\n"))
            }
            ConsistencyErrors(e) => format!("❌Consistency Error❌\n\n{}", e.join("\n\n")),
            JavaException(e) => format!("❌Java Exception❌\n\n{}", e),
        }
    }

    pub fn pretty_print(&self, program: viper::Program) -> String {
        self.utils.pretty_print(program)
    }
}

pub trait ViperUtils<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>);
}

impl<'a> ViperUtils<'a> for AstFactory<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>) {
        (self.local_var_decl(name, typ), self.local_var(name, typ))
    }
}

pub struct Position {
    begin: (i32, i32),
    end: (i32, i32),
}

impl FromStr for Position {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"\((\d+):(\d+) (\d+):(\d+)\)")?;
        if let Some(caps) = re.captures(s) {
            let v = caps
                .extract::<4>()
                .1
                .iter()
                .map(|n| n.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Self {
                begin: (v[0], v[1]),
                end: (v[2], v[3]),
            });
        }
        Err(anyhow!("Failed to parse Position, got {}", s))
    }
}

pub struct AstWrapper<T> {
    inner: T,
    position: Position,
}

impl<T> AstWrapper<T> {
    pub fn new(inner: T, pos: &str) -> anyhow::Result<Self> {
        Ok(Self {
            inner,
            position: Position::from_str(pos)?,
        })
    }
}
