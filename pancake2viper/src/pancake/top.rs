use super::{
    parse_stmt,
    shape::{parse_shape, Shape},
    Stmt,
};
use crate::parser::SExpr::{self, *};
use anyhow::anyhow;

#[derive(Debug)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<String>,
    pub body: Stmt,
}

pub fn parse_fn_dec(s: SExpr) -> anyhow::Result<FnDec> {
    match s {
        List(l) => match &l[..] {
            [Symbol(fun_dec), Symbol(name), List(args), List(body)] if fun_dec == "func" => {
                for arg in args {
                    parse_arg(arg)?;
                }
                Ok(FnDec {
                    fname: name.clone(),
                    args: vec![],
                    body: parse_stmt(body)?,
                })
            }
            _ => Err(anyhow!("Shape of SExpr::List does not match")),
        },
        _ => Err(anyhow!("SExpr is not a list")),
    }
}

#[derive(Debug)]
struct Arg {
    name: String,
    shape: Shape,
}

pub struct Program {
    pub functions: Vec<FnDec>,
}

fn parse_arg(s: &SExpr) -> anyhow::Result<Arg> {
    match s {
        List(args) => match &args[..] {
            [Symbol(name), Symbol(colon), Symbol(shape)] if colon == ":" => Ok(Arg {
                name: name.clone(),
                shape: parse_shape(shape)?,
            }),
            [Symbol(name), Symbol(colon), Int(shape)] if colon == ":" => Ok(Arg {
                name: name.clone(),
                shape: parse_shape(&shape.to_string())?,
            }),
            _ => Err(anyhow!("Could not parse argument")),
        },
        _ => Err(anyhow!("Could not parse argument")),
    }
}
