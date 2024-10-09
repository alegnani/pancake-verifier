use super::{Annotation, Decl, Expr, Stmt, Type};
use crate::{ir_to_viper::ViperEncodeCtx, shape::Shape, ToShape};

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
}

impl<'a> ToShape<'a> for FnDec {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
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
pub struct Predicate {
    pub name: String,
    pub args: Vec<Decl>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<Decl>,
    pub typ: Type,
    pub preposts: Vec<Annotation>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct AbstractMethod {
    pub name: String,
    pub args: Vec<Decl>,
    pub preposts: Vec<Annotation>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
    pub predicates: Vec<Predicate>,
    pub viper_functions: Vec<Function>,
    pub methods: Vec<AbstractMethod>,
}
