use std::str::FromStr;

use crate::SExpr;
use anyhow::anyhow;
use pest::Parser;
use pest_derive::Parser;

#[derive(Debug)]
pub enum Expr {
    Const(u64),
    Var(String),
    Label(String),
    Struct(Vec<Expr>),
    Field(u64, Box<Expr>),
    Load(String, Box<Expr>),
    LoadByte(Box<Expr>),
    Op(OpType, Vec<Expr>),
    Cmp(String, Box<Expr>, Box<Expr>),
    Shift(ShiftType, Box<Expr>, u64),
    BaseAddr,
}

#[derive(EnumString, Debug)]
enum OpType {
    Add,
    Sub,
    Mul,
    NotEqual,
    Equal,
    And,
    Or,
    Xor,
}

#[derive(Debug, EnumString)]
enum ShiftType {
    Lsl,
    Asr,
    Lsr,
}

#[derive(Debug)]
pub enum Stmt {
    Skip,
    Dec(String, Expr, Box<Stmt>),
    Assign(String, Expr),
    Store(Expr, Expr),
    StoreByte(Expr, Expr),
    Seq(Vec<Stmt>),
    If(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Break,
    Continue,
    Call(String, Expr, Vec<Expr>),
    Raise(String, Expr),
    Return(Expr),
    Tick,
    ExtCall(String, Expr, Expr, Expr, Expr),
}

pub struct FnDec {
    pub name: String,
    pub args: Vec<String>,
    pub body: Stmt,
}

use strum::EnumString;
use SExpr::*;

pub fn parse_fn_dec(s: SExpr) -> anyhow::Result<FnDec> {
    match s {
        SList(l) => match &l[..] {
            [SSymbol(fun_dec), SSymbol(name), SList(args), SList(body)] if fun_dec == "func" => {
                println!("Parsed function declaration of {}", name);
                // println!("Body: {:?}", body);
                println!("ARGS: {:?}", args);
                for arg in args {
                    parse_arg(arg).unwrap();
                }
                Ok(FnDec {
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

pub fn parse_stmt(s: &[SExpr]) -> anyhow::Result<Stmt> {
    println!("\nParsing stmt: {:?}", s);
    match s {
        [SSymbol(op)] if op == "skip" => Ok(Stmt::Skip),
        // Variable declaration
        [SSymbol(op), SList(decl), SList(rem)] if op == "dec" => {
            Ok(Stmt::Seq(vec![parse_dec(decl)?, parse_stmt(rem)?]))
        }
        [SSymbol(op), SList(decl)] if op == "dec" => parse_dec(decl),
        [SSymbol(var), SSymbol(eq), SList(exp)] if eq == ":=" => {
            Ok(Stmt::Assign(var.clone(), parse_exp(exp)?))
        }
        [SSymbol(op), SList(addr), SSymbol(eq), SList(exp)] if op == "mem" && eq == ":=" => {
            Ok(Stmt::Store(parse_exp(addr)?, parse_exp(exp)?))
        }
        [SSymbol(op), SList(addr), SSymbol(eq), SSymbol(byte), SList(exp)]
            if op == "mem" && eq == ":=" && byte == "byte" =>
        {
            Ok(Stmt::StoreByte(parse_exp(addr)?, parse_exp(exp)?))
        }
        [SSymbol(op), stmts @ ..] if op == "seq" => parse_seq(stmts),
        [SSymbol(op), SList(cond), SList(b1), SList(b2)] if op == "if" => Ok(Stmt::If(
            parse_exp(cond)?,
            Box::new(parse_stmt(b1)?),
            Box::new(parse_stmt(b2)?),
        )),
        [SSymbol(op), SList(cond), SList(b1), SSymbol(skip)] if op == "if" && skip == "skip" => {
            Ok(Stmt::If(
                parse_exp(cond)?,
                Box::new(parse_stmt(b1)?),
                Box::new(Stmt::Skip),
            ))
        }
        [SSymbol(op), SList(cond), SList(body)] if op == "while" => {
            Ok(Stmt::While(parse_exp(cond)?, Box::new(parse_stmt(body)?)))
        }
        [SSymbol(op)] if op == "break" => Ok(Stmt::Break),
        [SSymbol(op)] if op == "continue" => Ok(Stmt::Continue),
        [SSymbol(op), SList(label), SList(args), SSymbol(ret)] if op == "call" => Ok(Stmt::Call(
            "todo".into(),
            parse_exp(label)?,
            parse_exp_list(args)?,
        )),
        [SSymbol(op), SList(exp)] if op == "return" => Ok(Stmt::Return(parse_exp(exp)?)),
        [SSymbol(op)] if op == "tick" => Ok(Stmt::Tick),
        [SSymbol(op), SSymbol(name), SList(arg0), SList(arg1), SList(arg2), SList(arg3)]
            if op == "ext_call" =>
        {
            Ok(Stmt::ExtCall(
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

fn parse_dec(s: &[SExpr]) -> anyhow::Result<Stmt> {
    match s {
        [SSymbol(var), SSymbol(eq), SList(exp)] if eq == ":=" => Ok(Stmt::Dec(
            var.clone(),
            parse_exp(exp)?,
            Box::new(Stmt::Skip), // FIXME: ?
        )),
        _ => Err(anyhow!("Not a valid declaration")),
    }
}

fn parse_seq(s: &[SExpr]) -> anyhow::Result<Stmt> {
    let mut stmts = vec![];
    for stmt in s {
        match stmt {
            SList(stmt) => stmts.push(parse_stmt(stmt)?),
            _ => return Err(anyhow!("Error whilst parsing sequence")),
        }
    }
    Ok(Stmt::Seq(stmts))
}

pub fn parse_exp(s: &[SExpr]) -> anyhow::Result<Expr> {
    // println!("\nParsing expr: {:?}", s);
    match s {
        [SSymbol(cons), SSymbol(word)] if cons == "Const" && word.starts_with("0x") => {
            Ok(Expr::Const(u64::from_str_radix(&word[2..], 16)?))
        }
        [SSymbol(var), SSymbol(name)] if var == "Var" => Ok(Expr::Var(name.clone())),
        [SSymbol(label), SSymbol(name)] if label == "Label" => Ok(Expr::Label(name.clone())),
        [SSymbol(struc), exps @ ..] if struc == "Struct" => Ok(Expr::Struct(parse_exp_list(exps)?)),
        [SSymbol(field), SInt(idx), SList(exp)] if field == "Field" => {
            Ok(Expr::Field(*idx, Box::new(parse_exp(exp)?)))
        }
        [SSymbol(memloadbyte), SList(exp)] if memloadbyte == "MemLoadByte" => {
            Ok(Expr::LoadByte(Box::new(parse_exp(exp)?)))
        }
        [SSymbol(memload), SSymbol(shape), SList(exp)] if memload == "MemLoad" => {
            Ok(Expr::Load(shape.clone(), Box::new(parse_exp(exp)?))) // FIXME: do parsing of shape
        }
        [SSymbol(memload), SInt(shape), SList(exp)] if memload == "MemLoad" => {
            Ok(Expr::Load(shape.to_string(), Box::new(parse_exp(exp)?))) // FIXME: do parsing of shape
        }
        [SSymbol(shift), SList(exp), SInt(num)] => Ok(Expr::Shift(
            ShiftType::from_str(shift)?,
            Box::new(parse_exp(exp)?),
            *num,
        )),
        [SSymbol(base)] if base == "BaseAddr" => Ok(Expr::BaseAddr),
        [SSymbol(op), exps @ ..] => Ok(Expr::Op(OpType::from_str(op)?, parse_exp_list(exps)?)),
        _ => {
            println!("\n\nCould not parse: {:?}", s);
            panic!()
        }
    }
}

fn parse_exp_list(s: &[SExpr]) -> anyhow::Result<Vec<Expr>> {
    let mut l = vec![];
    for exp in s {
        match exp {
            SList(exp) => l.push(parse_exp(exp)?),
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
        SList(args) => match &args[..] {
            [SSymbol(name), SSymbol(colon), SSymbol(shape)] if colon == ":" => Ok(Arg {
                name: name.clone(),
                shape: parse_shape(shape)?,
            }),
            [SSymbol(name), SSymbol(colon), SInt(shape)] if colon == ":" => Ok(Arg {
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
