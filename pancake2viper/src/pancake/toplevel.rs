use super::Stmt;
use crate::shape::Shape;

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub shape: Shape,
}

#[derive(Debug, Clone)]
pub struct Predicate {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
    pub predicates: Vec<Predicate>,
    pub viper_functions: Vec<Function>,
    pub methods: Vec<Method>,
}
