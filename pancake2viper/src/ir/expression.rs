use strum::EnumString;

use crate::utils::Shape;

use super::{MemOpBytes, Type};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Const(i64),
    BoolLit(bool),
    Var(String),
    Label(String),
    Struct(Struct),
    Field(Field),
    Load(Load),
    LoadBits(LoadBits),
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
    UnfoldingIn(UnfoldingIn),
    Ternary(Ternary),
    AccessSlice(AccessSlice),
    Old(Old),
    ViperFieldAccess(ViperFieldAccess),
    SeqLength(SeqLength),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub field_idx: usize,
    pub obj: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Load {
    pub shape: Shape,
    pub address: Box<Expr>,
    pub assert: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoadBits {
    pub address: Box<Expr>,
    pub size: MemOpBytes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodCall {
    pub fname: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub fname: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Permission {
    Write,
    Read,
    Wildcard,
    Fractional(i64, i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccessPredicate {
    pub field: Box<Expr>,
    pub perm: Permission,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    SignedGt,
    SignedGte,
    SignedLt,
    SignedLte,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinOp {
    pub optype: BinOpType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpType {
    Neg,
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnOp {
    pub optype: UnOpType,
    pub right: Box<Expr>,
}

#[derive(EnumString, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShiftType {
    Lsl,
    Asr,
    Lsr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Shift {
    pub shifttype: ShiftType,
    pub value: Box<Expr>,
    pub amount: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decl {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Quantifier {
    Forall,
    Exists,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Quantified {
    pub quantifier: Quantifier,
    pub decls: Vec<Decl>,
    pub triggers: Vec<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayAccess {
    pub obj: Box<Expr>,
    pub idx: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldAccessChain {
    pub obj: Box<Expr>,
    pub idxs: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnfoldingIn {
    pub pred: Box<Expr>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ternary {
    pub cond: Box<Expr>,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SliceType {
    Inclusive,
    Exclusive,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccessSlice {
    pub field: Box<Expr>,
    pub typ: SliceType,
    pub lower: Box<Expr>,
    pub upper: Box<Expr>,
    pub perm: Permission,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Old {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ViperFieldAccess {
    pub obj: Box<Expr>,
    pub field: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqLength {
    pub expr: Box<Expr>,
}
