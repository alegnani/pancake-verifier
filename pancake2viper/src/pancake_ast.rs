use std::str::FromStr;

use crate::parser::SExpr;
use anyhow::anyhow;
use pest::Parser;
use pest_derive::Parser;

#[derive(Debug, Clone)]
pub enum PancakeExpr {
    Const(i64),
    Var(String),
    Label(String),
    Struct(Vec<PancakeExpr>),
    Field(u64, Box<PancakeExpr>),
    Load(String, Box<PancakeExpr>),
    LoadByte(Box<PancakeExpr>),
    Op(PancakeOpType, Vec<PancakeExpr>),
    Cmp(String, Box<PancakeExpr>, Box<PancakeExpr>),
    Shift(PancakeShiftType, Box<PancakeExpr>, u64),
    BaseAddr,
}

#[derive(EnumString, Debug, Clone, Copy)]
pub enum PancakeOpType {
    Add,
    Sub,
    Mul,
    NotEqual,
    Equal,
    And,
    Or,
    Xor,
}

#[derive(Debug, EnumString, Clone, Copy)]
pub enum PancakeShiftType {
    Lsl,
    Asr,
    Lsr,
}

#[derive(Debug)]
pub enum PancakeStmt {
    Skip,
    Dec(String, PancakeExpr, Box<PancakeStmt>),
    Assign(String, PancakeExpr),
    Store(PancakeExpr, PancakeExpr),
    StoreByte(PancakeExpr, PancakeExpr),
    Seq(Vec<PancakeStmt>),
    If(PancakeExpr, Box<PancakeStmt>, Box<PancakeStmt>),
    While(PancakeExpr, Box<PancakeStmt>),
    Break,
    Continue,
    Call(String, PancakeExpr, Vec<PancakeExpr>),
    Raise(String, PancakeExpr),
    Return(PancakeExpr),
    Tick,
    ExtCall(String, PancakeExpr, PancakeExpr, PancakeExpr, PancakeExpr),
}

pub struct PancakeFnDec {
    pub name: String,
    pub args: Vec<String>,
    pub body: PancakeStmt,
}

use strum::EnumString;
use SExpr::*;

pub fn parse_fn_dec(s: SExpr) -> anyhow::Result<PancakeFnDec> {
    match s {
        List(l) => match &l[..] {
            [Symbol(fun_dec), Symbol(name), List(args), List(body)] if fun_dec == "func" => {
                println!("Parsed function declaration of {}", name);
                // println!("Body: {:?}", body);
                println!("ARGS: {:?}", args);
                for arg in args {
                    parse_arg(arg).unwrap();
                }
                Ok(PancakeFnDec {
                    name: name.clone(),
                    args: vec![],
                    body: parse_stmt(body)?,
                })
            }
            _ => Err(anyhow!("Shape of SExpr::List does not match")),
        },
        _ => Err(anyhow!("SExpr is not a list")),
    }
}

pub fn parse_stmt(s: &[SExpr]) -> anyhow::Result<PancakeStmt> {
    println!("\nParsing stmt: {:?}", s);
    match s {
        [Symbol(op)] if op == "skip" => Ok(PancakeStmt::Skip),
        // Variable declaration
        [Symbol(op), List(decl), List(rem)] if op == "dec" => {
            Ok(PancakeStmt::Seq(vec![parse_dec(decl)?, parse_stmt(rem)?]))
        }
        [Symbol(op), List(decl)] if op == "dec" => parse_dec(decl),
        [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => {
            Ok(PancakeStmt::Assign(var.clone(), parse_exp(exp)?))
        }
        [Symbol(op), List(addr), Symbol(eq), List(exp)] if op == "mem" && eq == ":=" => {
            Ok(PancakeStmt::Store(parse_exp(addr)?, parse_exp(exp)?))
        }
        [Symbol(op), List(addr), Symbol(eq), Symbol(byte), List(exp)]
            if op == "mem" && eq == ":=" && byte == "byte" =>
        {
            Ok(PancakeStmt::StoreByte(parse_exp(addr)?, parse_exp(exp)?))
        }
        [Symbol(op), stmts @ ..] if op == "seq" => parse_seq(stmts),
        [Symbol(op), List(cond), List(b1), List(b2)] if op == "if" => Ok(PancakeStmt::If(
            parse_exp(cond)?,
            Box::new(parse_stmt(b1)?),
            Box::new(parse_stmt(b2)?),
        )),
        [Symbol(op), List(cond), List(b1), Symbol(skip)] if op == "if" && skip == "skip" => {
            Ok(PancakeStmt::If(
                parse_exp(cond)?,
                Box::new(parse_stmt(b1)?),
                Box::new(PancakeStmt::Skip),
            ))
        }
        [Symbol(op), List(cond), List(body)] if op == "while" => Ok(PancakeStmt::While(
            parse_exp(cond)?,
            Box::new(parse_stmt(body)?),
        )),
        [Symbol(op)] if op == "break" => Ok(PancakeStmt::Break),
        [Symbol(op)] if op == "continue" => Ok(PancakeStmt::Continue),
        [Symbol(op), List(label), List(args), Symbol(ret)] if op == "call" => Ok(
            PancakeStmt::Call("todo".into(), parse_exp(label)?, parse_exp_list(args)?),
        ),
        [Symbol(op), List(exp)] if op == "return" => Ok(PancakeStmt::Return(parse_exp(exp)?)),
        [Symbol(op)] if op == "tick" => Ok(PancakeStmt::Tick),
        [Symbol(op), Symbol(name), List(arg0), List(arg1), List(arg2), List(arg3)]
            if op == "ext_call" =>
        {
            Ok(PancakeStmt::ExtCall(
                name.clone(),
                parse_exp(arg0)?,
                parse_exp(arg1)?,
                parse_exp(arg2)?,
                parse_exp(arg3)?,
            ))
        }
        _ => panic!(),
    }
}

fn parse_dec(s: &[SExpr]) -> anyhow::Result<PancakeStmt> {
    match s {
        [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => Ok(PancakeStmt::Dec(
            var.clone(),
            parse_exp(exp)?,
            Box::new(PancakeStmt::Skip), // FIXME: ?
        )),
        _ => Err(anyhow!("Not a valid declaration")),
    }
}

fn parse_seq(s: &[SExpr]) -> anyhow::Result<PancakeStmt> {
    let mut stmts = vec![];
    for stmt in s {
        match stmt {
            List(stmt) => stmts.push(parse_stmt(stmt)?),
            _ => return Err(anyhow!("Error whilst parsing sequence")),
        }
    }
    Ok(PancakeStmt::Seq(stmts))
}

pub fn parse_exp(s: &[SExpr]) -> anyhow::Result<PancakeExpr> {
    // println!("\nParsing expr: {:?}", s);
    match s {
        [Symbol(cons), Symbol(word)] if cons == "Const" && word.starts_with("0x") => {
            Ok(PancakeExpr::Const(i64::from_str_radix(&word[2..], 16)?))
        }
        [Symbol(var), Symbol(name)] if var == "Var" => Ok(PancakeExpr::Var(name.clone())),
        [Symbol(label), Symbol(name)] if label == "Label" => Ok(PancakeExpr::Label(name.clone())),
        [Symbol(struc), exps @ ..] if struc == "Struct" => {
            Ok(PancakeExpr::Struct(parse_exp_list(exps)?))
        }
        [Symbol(field), Int(idx), List(exp)] if field == "Field" => {
            Ok(PancakeExpr::Field(*idx, Box::new(parse_exp(exp)?)))
        }
        [Symbol(memloadbyte), List(exp)] if memloadbyte == "MemLoadByte" => {
            Ok(PancakeExpr::LoadByte(Box::new(parse_exp(exp)?)))
        }
        [Symbol(memload), Symbol(shape), List(exp)] if memload == "MemLoad" => {
            Ok(PancakeExpr::Load(shape.clone(), Box::new(parse_exp(exp)?))) // FIXME: do parsing of shape
        }
        [Symbol(memload), Int(shape), List(exp)] if memload == "MemLoad" => {
            Ok(PancakeExpr::Load(
                shape.to_string(),
                Box::new(parse_exp(exp)?),
            )) // FIXME: do parsing of shape
        }
        [Symbol(shift), List(exp), Int(num)] => Ok(PancakeExpr::Shift(
            PancakeShiftType::from_str(shift)?,
            Box::new(parse_exp(exp)?),
            *num,
        )),
        [Symbol(base)] if base == "BaseAddr" => Ok(PancakeExpr::BaseAddr),
        [Symbol(op), exps @ ..] => Ok(PancakeExpr::Op(
            PancakeOpType::from_str(op)?,
            parse_exp_list(exps)?,
        )),
        _ => {
            println!("\n\nCould not parse: {:?}", s);
            panic!()
        }
    }
}

fn parse_exp_list(s: &[SExpr]) -> anyhow::Result<Vec<PancakeExpr>> {
    let mut l = vec![];
    for exp in s {
        match exp {
            List(exp) => l.push(parse_exp(exp)?),
            _ => return Err(anyhow!("Error whilst parsing expr list")),
        }
    }
    Ok(l)
}

#[derive(Parser)]
#[grammar = "shape.pest"]
struct ShapeParser;

#[derive(Debug, Clone)]
enum Shape {
    Simple(u64),
    Nested(Vec<Shape>),
}

#[derive(Debug)]
struct Arg {
    name: String,
    shape: Shape,
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

fn parse_shape(s: &str) -> anyhow::Result<Shape> {
    // if s == "1" {
    //     return Ok(Shape::Simple(1));
    // }
    // let mut counter = 0;
    // let mut stack = vec![];
    // for c in s.chars() {
    //     match c {
    //         '<' => {
    //             counter = 0;
    //             stack.push(vec![]);
    //         }
    //         '1' => counter += 1,
    //         ',' => (),
    //         '>' => ,
    //         '_' => panic!(),
    //     }
    // }
    // todo!()
    let top = ShapeParser::parse(Rule::TOP, s)?;
    println!("top: {:?}", top);
    let mut shape = traverse_shape(top);
    reduce_shape(&mut shape);
    println!("Shape: {:?}", shape);
    Ok(shape)
}

fn traverse_shape(pairs: pest::iterators::Pairs<Rule>) -> Shape {
    let mut counter = 0;
    // println!("Called traverse: {:?}", pairs);
    let mut buf = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::NESTED => {
                if counter != 0 {
                    buf.push(Shape::Simple(counter));
                    counter = 0;
                }
                println!("NESTED");
                let nested = traverse_shape(pair.into_inner());
                buf.push(nested);
            }
            _ => {
                println!("SIMPLE");
                counter += 1;
            }
        }
    }
    println!("Counter: {}", counter);
    if buf.is_empty() {
        Shape::Simple(counter)
    } else {
        if counter != 0 {
            buf.push(Shape::Simple(counter));
        }
        Shape::Nested(buf)
    }
}

fn reduce_shape(shape: &mut Shape) {
    match shape {
        Shape::Nested(vec) if vec.len() == 1 => {
            if let Shape::Simple(x) = vec[0] {
                *shape = Shape::Simple(x)
            }
        }
        Shape::Nested(vec) => {
            for entry in vec {
                reduce_shape(entry);
            }
        }
        _ => (),
    }
}
