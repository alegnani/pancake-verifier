use super::shape::Shape;
use strum::EnumString;

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i64),
    Var(String),
    Label(String),
    Struct(Struct),
    Field(Field),
    Load(Load),
    LoadByte(LoadByte),
    Op(Op),
    Shift(Shift),
    BaseAddr,
    BytesInWord,
    Call(ExprCall),
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

#[derive(EnumString, Debug, Clone, Copy)]
pub enum OpType {
    Add,
    Sub,
    Mul,
    NotEqual,
    Equal,
    Less,
    NotLess,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
pub struct Op {
    pub optype: OpType,
    pub operands: Vec<Expr>,
}

#[derive(Debug, EnumString, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub rettype: Shape,
    pub fname: Box<Expr>,
    pub args: Vec<Expr>,
}
