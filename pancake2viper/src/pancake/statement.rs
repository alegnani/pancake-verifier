use super::{parse_exp, parse_exp_list, Expr};
use crate::parser::SExpr::{self, *};
use anyhow::anyhow;

#[derive(Debug, Clone)]
pub enum Stmt {
    Skip,
    Declaration(Declaration),
    Assign(Assign),
    Store(Store),
    StoreByte(StoreByte),
    Seq(Seq),
    If(If),
    While(While),
    Break,
    Continue,
    Call(Call),
    TailCall(TailCall),
    ExtCall(ExtCall),
    Raise(Raise),
    Return(Return),
    Tick,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub lhs: String,
    pub rhs: Expr,
    pub scope: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: String,
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub struct Store {
    pub address: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct StoreByte {
    pub address: Expr,
    pub value: Expr,
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
    pub rettype: String,
    pub fname: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct TailCall {
    pub fname: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExtCall {
    pub fname: String,
    pub args: [Expr; 4],
}

#[derive(Debug, Clone)]
pub struct Raise {
    pub error: String,
    pub idk: Expr,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Expr,
}

pub fn parse_stmt_symbol(symbol: &str) -> anyhow::Result<Stmt> {
    match symbol {
        "break" => Ok(Stmt::Break),
        "continue" => Ok(Stmt::Continue),
        "skip" => Ok(Stmt::Skip),
        "tick" => Ok(Stmt::Tick),
        x => Err(anyhow!("Failed to parse stmt symbol: {}", x)),
    }
}

pub fn parse_stmt(s: &[SExpr]) -> anyhow::Result<Stmt> {
    match s {
        [Symbol(op)] => parse_stmt_symbol(op),
        // Variable declaration
        [Symbol(op), List(decl), List(rem)] if op == "dec" => parse_dec(decl, Some(rem)),
        [Symbol(op), List(decl)] if op == "dec" => parse_dec(decl, None),
        [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => Ok(Stmt::Assign(Assign {
            lhs: var.clone(),
            rhs: parse_exp(exp)?,
        })),
        [Symbol(op), List(addr), Symbol(eq), List(exp)] if op == "mem" && eq == ":=" => {
            Ok(Stmt::Store(Store {
                address: parse_exp(addr)?,
                value: parse_exp(exp)?,
            }))
        }
        [Symbol(op), List(addr), Symbol(eq), Symbol(byte), List(exp)]
            if op == "mem" && eq == ":=" && byte == "byte" =>
        {
            Ok(Stmt::StoreByte(StoreByte {
                address: parse_exp(addr)?,
                value: parse_exp(exp)?,
            }))
        }
        [Symbol(op), stmts @ ..] if op == "seq" => parse_seq(stmts),

        // if
        [Symbol(op), List(cond), List(b1), List(b2)] if op == "if" => Ok(Stmt::If(If {
            cond: parse_exp(cond)?,
            if_branch: Box::new(parse_stmt(b1)?),
            else_branch: Box::new(parse_stmt(b2)?),
        })),
        [Symbol(op), List(cond), List(b1), Symbol(b2)] if op == "if" => Ok(Stmt::If(If {
            cond: parse_exp(cond)?,
            if_branch: Box::new(parse_stmt(b1)?),
            else_branch: Box::new(parse_stmt_symbol(b2)?),
        })),
        [Symbol(op), List(cond), Symbol(b1), List(b2)] if op == "if" => Ok(Stmt::If(If {
            cond: parse_exp(cond)?,
            if_branch: Box::new(parse_stmt_symbol(b1)?),
            else_branch: Box::new(parse_stmt(b2)?),
        })),
        [Symbol(op), List(cond), Symbol(b1), Symbol(b2)] if op == "if" => Ok(Stmt::If(If {
            cond: parse_exp(cond)?,
            if_branch: Box::new(parse_stmt_symbol(b1)?),
            else_branch: Box::new(parse_stmt_symbol(b2)?),
        })),

        // while
        [Symbol(op), List(cond), List(body)] if op == "while" => Ok(Stmt::While(While {
            cond: parse_exp(cond)?,
            body: Box::new(parse_stmt(body)?),
        })),
        [Symbol(op), List(cond), Symbol(body)] if op == "while" => Ok(Stmt::While(While {
            cond: parse_exp(cond)?,
            body: Box::new(parse_stmt_symbol(body)?),
        })),
        [Symbol(op), List(label), List(args), Symbol(ret)] if op == "call" => {
            Ok(Stmt::Call(Call {
                rettype: ret.into(),
                fname: parse_exp(label)?,
                args: parse_exp_list(args)?,
            }))
        }
        [Symbol(op), List(label), List(args), Int(ret)] if op == "call" => Ok(Stmt::Call(Call {
            rettype: "todo_call".into(),
            fname: parse_exp(label)?,
            args: parse_exp_list(args)?,
        })),
        [Symbol(op), List(exp)] if op == "return" => Ok(Stmt::Return(Return {
            value: parse_exp(exp)?,
        })),
        [Symbol(op), Symbol(name), List(arg0), List(arg1), List(arg2), List(arg3)]
            if op == "ext_call" =>
        {
            Ok(Stmt::ExtCall(ExtCall {
                fname: name.clone(),
                args: [
                    parse_exp(arg0)?,
                    parse_exp(arg1)?,
                    parse_exp(arg2)?,
                    parse_exp(arg3)?,
                ],
            }))
        }
        [Symbol(op), List(label), List(args)] if op == "tail_call" => {
            Ok(Stmt::TailCall(TailCall {
                fname: parse_exp(label)?,
                args: parse_exp_list(args)?,
            }))
        }
        x => panic!("Could not parse stmt: {:?}", x),
    }
}

fn parse_dec(decl: &[SExpr], scope: Option<&[SExpr]>) -> anyhow::Result<Stmt> {
    let scope = match scope {
        Some(stmts) => parse_stmt(stmts)?,
        None => Stmt::Skip,
    };
    match decl {
        [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => Ok(Stmt::Declaration(Declaration {
            lhs: var.clone(),
            rhs: parse_exp(exp)?,
            scope: Box::new(scope),
        })),
        _ => Err(anyhow!("Not a valid declaration")),
    }
}

fn parse_seq(s: &[SExpr]) -> anyhow::Result<Stmt> {
    let mut stmts = vec![];
    for stmt in s {
        match stmt {
            List(stmt) => stmts.push(parse_stmt(stmt)?),
            Symbol(stmt) => stmts.push(parse_stmt_symbol(stmt)?),
            _ => return Err(anyhow!("Error whilst parsing sequence")),
        }
    }
    Ok(Stmt::Seq(Seq { stmts }))
}
