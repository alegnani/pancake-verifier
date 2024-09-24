use super::parser::parse_annot;

#[derive(Debug, Clone, Copy)]
pub enum AnnotationType {
    Precondition,
    Postcondition,
    Assertion,
    Invariant,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub typ: AnnotationType,
    pub expr: Expr,
}

impl Annotation {
    pub fn parse(annot: &str) -> Self {
        parse_annot(annot)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    UnOp(UnOp),
    BinOp(BinOp),
    Quantified(Quantified),
    Const(i64),
    Call(Call),
}

#[derive(Debug, Clone, Copy)]
pub enum UnOperator {
    Neg,
    Minus,
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub op: UnOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    Imp,
    Iff,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOperator,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Bool,
    Int,
    IArray,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum Quantifier {
    Forall,
    Exists,
}

#[derive(Debug, Clone)]
pub struct Quantified {
    pub quantifier: Quantifier,
    pub decls: Vec<Decl>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub fname: String,
    pub args: Vec<Expr>,
}
