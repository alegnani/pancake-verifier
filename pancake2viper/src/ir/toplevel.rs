use super::{Annotation, Decl, Expr, Stmt, Type};

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
    pub retvar: String,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Predicate {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<Arg>,
    pub typ: Type,
    pub preposts: Vec<Annotation>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct AbstractMethod {
    pub name: String,
    pub args: Vec<Arg>,
    pub rettyps: Vec<Decl>,
    pub preposts: Vec<Annotation>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
    pub predicates: Vec<Predicate>,
    pub viper_functions: Vec<Function>,
    pub methods: Vec<AbstractMethod>,
}
