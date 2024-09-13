use std::str::FromStr;

use crate::parser::SExpr::{self, *};
use anyhow::anyhow;
use strum::EnumString;

use super::shape::Shape;

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
    // Cmp(String, Box<Expr>, Box<Expr>),
    Shift(Shift),
    BaseAddr,
    Call(ExprCall),
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub elements: Vec<Expr>,
}

impl Struct {
    pub fn new(elements: Vec<Expr>) -> Self {
        Self { elements }
    }
    // pub fn flatten(&self) -> Vec<Expr> {
    //     let mut result = Vec::new();
    //     Self::flatten_helper(&self.elements, &mut result);
    //     result
    // }

    // fn flatten_helper(list: &[Expr], result: &mut Vec<Expr>) {
    //     for expr in list {
    //         match expr {
    //             Expr::Struct(inner) => Self::flatten_helper(&inner.0, result),
    //             x => result.push(x.to_owned()),
    //         }
    //     }
    // }
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
}

#[derive(Debug, Clone)]
pub struct LoadByte {
    pub address: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Op {
    pub optype: OpType,
    pub operands: Vec<Expr>,
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

#[derive(Debug, EnumString, Clone, Copy)]
pub enum ShiftType {
    Lsl,
    Asr,
    Lsr,
}

pub fn parse_exp(s: &[SExpr]) -> anyhow::Result<Expr> {
    match s {
        [Symbol(cons), Symbol(word)] if cons == "Const" && word.starts_with("0x") => {
            Ok(Expr::Const(i64::from_str_radix(&word[2..], 16)?))
        }
        [Symbol(var), Symbol(name)] if var == "Var" => Ok(Expr::Var(name.clone())),
        [Symbol(label), Symbol(name)] if label == "Label" => Ok(Expr::Label(name.clone())),
        [Symbol(struc), exps @ ..] if struc == "Struct" => {
            Ok(Expr::Struct(Struct::new(parse_exp_list(exps)?)))
        }
        [Symbol(field), Int(idx), List(exp)] if field == "Field" => Ok(Expr::Field(Field {
            field_idx: *idx as usize,
            obj: Box::new(parse_exp(exp)?),
        })),
        [Symbol(memloadbyte), List(exp)] if memloadbyte == "MemLoadByte" => {
            Ok(Expr::LoadByte(LoadByte {
                address: Box::new(parse_exp(exp)?),
            }))
        }
        [Symbol(memload), Symbol(shape), List(exp)] if memload == "MemLoad" => {
            Ok(Expr::Load(Load {
                shape: Shape::parse(shape)?,
                address: Box::new(parse_exp(exp)?),
            }))
        }
        [Symbol(memload), Int(shape), List(exp)] if memload == "MemLoad" => Ok(Expr::Load(Load {
            shape: Shape::parse(&shape.to_string())?,
            address: Box::new(parse_exp(exp)?),
        })),
        [Symbol(shift), List(exp), Int(num)] => Ok(Expr::Shift(Shift {
            shifttype: ShiftType::from_str(shift)?,
            value: Box::new(parse_exp(exp)?),
            amount: *num,
        })),
        [Symbol(base)] if base == "BaseAddr" => Ok(Expr::BaseAddr),
        [Symbol(op), List(label), List(args), Symbol(ret)] if op == "call" => {
            Ok(Expr::Call(ExprCall {
                rettype: Shape::parse(ret)?,
                fname: Box::new(parse_exp(label)?),
                args: parse_exp_list(args)?,
            }))
        }
        [Symbol(op), List(label), List(args), Int(ret)] if op == "call" => {
            Ok(Expr::Call(ExprCall {
                rettype: Shape::Simple,
                fname: Box::new(parse_exp(label)?),
                args: parse_exp_list(args)?,
            }))
        }
        [Symbol(op), exps @ ..] => Ok(Expr::Op(Op {
            optype: OpType::from_str(op)?,
            operands: parse_exp_list(exps)?,
        })),
        _ => {
            panic!()
        }
    }
}

pub fn parse_exp_list(s: &[SExpr]) -> anyhow::Result<Vec<Expr>> {
    let mut l = vec![];
    for exp in s {
        match exp {
            List(exp) => l.push(parse_exp(exp)?),
            _ => return Err(anyhow!("Error whilst parsing expr list")),
        }
    }
    Ok(l)
}
