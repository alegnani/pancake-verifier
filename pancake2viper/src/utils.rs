use viper::{AstFactory, Expr, LocalVarDecl, Type, Viper};

use crate::{
    pancake,
    translation::{top::ViperEncodeCtx, ToViper},
};

pub fn pretty_print(viper: &Viper, program: pancake::Program) -> anyhow::Result<String> {
    let vctx = viper.attach_current_thread();
    let utils = vctx.new_ast_utils();
    let ast = vctx.new_ast_factory();
    let mut ctx = ViperEncodeCtx::new(ast);
    Ok(utils.pretty_print(program.to_viper(&mut ctx)))
}

pub trait ViperUtils<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>);
}

impl<'a> ViperUtils<'a> for AstFactory<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>) {
        (self.local_var_decl(name, typ), self.local_var(name, typ))
    }
}
