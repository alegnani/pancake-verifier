use std::time::Instant;

use viper::{
    smt_manager::SmtManager, AstFactory, AstUtils, Expr, LocalVarDecl, Type, VerificationContext,
    Verifier, Viper,
};

use super::ViperUtils;

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
        Self::from_handle(viper, z3_exe)
    }

    pub fn from_handle(viper: &'static viper::Viper, z3_exe: String) -> Self {
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
    pub fn verify(&mut self, program: viper::Program) -> (String, bool) {
        use viper::VerificationResult::*;
        let start = Instant::now();
        match self.verifier.verify(program) {
            Success => (
                format!(
                    "️✅Verification Successful in {:.2}s ✅",
                    start.elapsed().as_secs_f32()
                ),
                true,
            ),
            Failure(e) => {
                let errors = e.into_iter().map(|e| e.message).collect::<Vec<_>>();
                (
                    format!("❌Verification Error❌\n\n{}", errors.join("\n\n")),
                    false,
                )
            }
            ConsistencyErrors(e) => (
                format!("❌Consistency Error❌\n\n{}", e.join("\n\n")),
                false,
            ),
            JavaException(e) => (format!("❌Java Exception❌\n\n{}", e), false),
        }
    }

    pub fn pretty_print(&self, program: viper::Program) -> String {
        self.utils.pretty_print(program)
    }
}

impl<'a> ViperUtils<'a> for AstFactory<'a> {
    fn seq_slice(&self, seq: Expr<'a>, lower: Expr<'a>, upper: Expr<'a>) -> Expr<'a> {
        self.seq_drop(self.seq_take(seq, upper), lower)
    }

    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>) {
        (self.local_var_decl(name, typ), self.local_var(name, typ))
    }

    fn zero(&self) -> Expr<'a> {
        self.int_lit(0)
    }

    fn one(&self) -> Expr<'a> {
        self.int_lit(1)
    }

    fn two(&self) -> Expr<'a> {
        self.int_lit(2)
    }
}

//pub struct Position {
//    begin: (i32, i32),
//    end: (i32, i32),
//}
//
//impl FromStr for Position {
//    type Err = anyhow::Error;
//
//    fn from_str(s: &str) -> Result<Self, Self::Err> {
//        let re = Regex::new(r"\((\d+):(\d+) (\d+):(\d+)\)")?;
//        if let Some(caps) = re.captures(s) {
//            let v = caps
//                .extract::<4>()
//                .1
//                .iter()
//                .map(|n| n.parse::<i32>())
//                .collect::<Result<Vec<_>, _>>()?;
//            return Ok(Self {
//                begin: (v[0], v[1]),
//                end: (v[2], v[3]),
//            });
//        }
//        Err(anyhow!("Failed to parse Position, got {}", s))
//    }
//}
//
//pub struct AstWrapper<T> {
//    inner: T,
//    position: Position,
//}
//
//impl<T> AstWrapper<T> {
//    pub fn new(inner: T, pos: &str) -> anyhow::Result<Self> {
//        Ok(Self {
//            inner,
//            position: Position::from_str(pos)?,
//        })
//    }
//}
