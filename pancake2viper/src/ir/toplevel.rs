use super::Stmt;
use crate::{ir_to_viper::ViperEncodeCtx, shape::Shape, ToShape};

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
}

impl<'a> ToShape<'a> for FnDec {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        // let shapes = FnDec::collect_returns(&self.body, ctx);
        // TODO: add check for types to be the same and non-empty
        // shapes[0].clone()
        Shape::Simple
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub shape: Shape,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
}
