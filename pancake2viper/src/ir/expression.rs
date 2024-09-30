use strum::EnumString;

use crate::shape::Shape;

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i64),
    Var(String),
    Label(String),
    Struct(Struct),
    Field(Field),
    Load(Load),
    LoadByte(LoadByte),
    BinOp(BinOp),
    UnOp(UnOp),
    Shift(Shift),
    BaseAddr,
    BytesInWord,
    MethodCall(MethodCall),
    FunctionCall(FunctionCall),
    Quantified(Quantified),
    ArrayAccess(ArrayAccess),
    AccessPredicate(AccessPredicate),
    FieldAccessChain(FieldAccessChain),
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub field_idx: usize,
    pub obj: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Load {
    pub shape: Shape,
    pub address: Box<Expr>,
    pub assert: bool,
}

#[derive(Debug, Clone)]
pub struct LoadByte {
    pub address: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub rettype: Shape,
    pub fname: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub fname: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Permission {
    Write,
    Read,
    Wildcard,
    Fractional(i64, i64),
}

#[derive(Debug, Clone)]
pub struct AccessPredicate {
    pub field: Box<Expr>,
    pub perm: Permission,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpType {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    Imp,
    Iff,
    ViperEqual,
    ViperNotEqual,
    PancakeEqual,
    PancakeNotEqual,
    Gt,
    Gte,
    Lt,
    Lte,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub optype: BinOpType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpType {
    Neg,
    Minus,
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub optype: UnOpType,
    pub right: Box<Expr>,
}

#[derive(EnumString, Debug, Clone, Copy)]
pub enum ShiftType {
    Lsl,
    Asr,
    Lsr,
}

#[derive(Debug, Clone)]
pub struct Shift {
    pub shifttype: ShiftType,
    pub value: Box<Expr>,
    pub amount: u64,
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
    pub triggers: Vec<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub obj: Box<Expr>,
    pub idx: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FieldAccessChain {
    pub obj: Box<Expr>,
    pub idxs: Vec<usize>,
}
