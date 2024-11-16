use super::expression::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Skip,
    Annotation(Annotation),
    Definition(Definition),
    Assign(Assign),
    Store(Store),
    StoreBits(StoreBits),
    SharedStore(SharedStore),
    SharedStoreBits(SharedStoreBits),
    SharedLoad(SharedLoad),
    SharedLoadBits(SharedLoadBits),
    Seq(Seq),
    If(If),
    While(While),
    Break,
    Continue,
    Call(Call),
    ExtCall(ExtCall),
    Return,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub lhs: String,
    pub rhs: Expr,
    pub scope: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: String,
    pub rhs: Expr,
}

// Stores

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemOpBytes {
    Byte,
    HalfWord,
    QuarterWord,
    Word,
}

#[derive(Debug, Clone)]
pub struct Store {
    pub address: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct StoreBits {
    pub address: Expr,
    pub value: Expr,
    pub size: MemOpBytes,
}

#[derive(Debug, Clone)]
pub struct SharedStore {
    pub address: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct SharedStoreBits {
    pub address: Expr,
    pub value: Expr,
    pub size: MemOpBytes,
}

// Shared loads

#[derive(Debug, Clone)]
pub struct SharedLoad {
    pub address: Expr,
    pub dst: Expr,
}

#[derive(Debug, Clone)]
pub struct SharedLoadBits {
    pub address: Expr,
    pub dst: Expr,
    pub size: MemOpBytes,
}

#[derive(Debug, Clone)]
pub struct Seq {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub if_branch: Box<Stmt>,
    pub else_branch: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub call: Expr,
}

#[derive(Debug, Clone)]
pub struct ExtCall {
    pub fname: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Expr,
}

#[derive(Debug, Clone, Copy)]
pub enum AnnotationType {
    Precondition,
    Postcondition,
    Assertion,
    Refutation,
    Invariant,
    Assumption,
    Inhale,
    Exhale,
    Fold,
    Unfold,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub typ: AnnotationType,
    pub expr: Expr,
}
