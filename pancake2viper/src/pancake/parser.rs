use anyhow::anyhow;
use std::str::FromStr;

use super::*;
use crate::parser::SExpr::{self, *};

impl Expr {
    pub fn parse(s: &[SExpr]) -> anyhow::Result<Self> {
        match s {
            [Symbol(cons), Symbol(word)] if cons == "Const" && word.starts_with("0x") => {
                Ok(Self::Const(u64::from_str_radix(&word[2..], 16)? as i64))
            }
            [Symbol(var), Symbol(name)] if var == "Var" => Ok(Self::Var(name.clone())),
            [Symbol(label), Symbol(name)] if label == "Label" => Ok(Self::Label(name.clone())),
            [Symbol(struc), exps @ ..] if struc == "Struct" => {
                Ok(Self::Struct(Struct::new(Self::parse_slice(exps)?)))
            }
            [Symbol(field), Int(idx), List(exp)] if field == "Field" => Ok(Self::Field(Field {
                field_idx: *idx as usize,
                obj: Box::new(Self::parse(exp)?),
            })),
            [Symbol(memloadbyte), List(exp)] if memloadbyte == "MemLoadByte" => {
                Ok(Self::LoadByte(LoadByte {
                    address: Box::new(Self::parse(exp)?),
                }))
            }
            [Symbol(memload), Symbol(shape), List(exp)] if memload == "MemLoad" => {
                Ok(Self::Load(Load {
                    shape: Shape::parse(shape)?,
                    address: Box::new(Self::parse(exp)?),
                    assert: true,
                }))
            }
            [Symbol(memload), Int(shape), List(exp)] if memload == "MemLoad" => {
                Ok(Self::Load(Load {
                    shape: Shape::parse(&shape.to_string())?,
                    address: Box::new(Self::parse(exp)?),
                    assert: true,
                }))
            }
            [Symbol(shift), List(exp), Int(num)] => Ok(Self::Shift(Shift {
                shifttype: ShiftType::from_str(shift)?,
                value: Box::new(Self::parse(exp)?),
                amount: *num,
            })),
            [Symbol(base)] if base == "BaseAddr" => Ok(Self::BaseAddr),
            [Symbol(bytes)] if bytes == "BytesInWord" => Ok(Self::BytesInWord),
            [Symbol(op), List(label), List(args), Symbol(ret)] if op == "call" => {
                Ok(Self::Call(ExprCall {
                    rettype: Shape::Simple,
                    fname: Box::new(Self::parse(label)?),
                    args: Self::parse_slice(args)?,
                }))
            }
            [Symbol(op), List(label), List(args), Int(_)] if op == "call" => {
                Ok(Self::Call(ExprCall {
                    rettype: Shape::Simple,
                    fname: Box::new(Self::parse(label)?),
                    args: Self::parse_slice(args)?,
                }))
            }
            [Symbol(op), exps @ ..] => Ok(Self::Op(Op {
                optype: OpType::from_str(op)?,
                operands: Self::parse_slice(exps)?,
            })),
            _ => {
                panic!()
            }
        }
    }

    fn parse_slice(s: &[SExpr]) -> anyhow::Result<Vec<Self>> {
        let mut l = vec![];
        for exp in s {
            match exp {
                List(exp) => l.push(Self::parse(exp)?),
                _ => return Err(anyhow!("Error whilst parsing expr list")),
            }
        }
        Ok(l)
    }
}

impl Stmt {
    pub fn parse(s: Vec<&SExpr>) -> anyhow::Result<Self> {
        match &s[..] {
            [Symbol(op), SString(at), SString(annot)] if op == "annot" && at == "@" => {
                Ok(Self::Annotation(Annotation {
                    line: annot.to_owned(),
                }))
            }
            [Symbol(op)] => Self::parse_symbol(op),
            // Variable declaration
            [Symbol(op), List(decl), List(rem)] if op == "dec" => Self::parse_dec(
                decl.iter().collect::<Vec<_>>(),
                Some(rem.iter().collect::<Vec<_>>()),
            ),
            [Symbol(op), List(decl)] if op == "dec" => {
                Self::parse_dec(decl.iter().collect::<Vec<_>>(), None)
            }
            [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => Ok(Self::Assign(Assign {
                lhs: var.clone(),
                rhs: Expr::parse(exp)?,
            })),
            [Symbol(op), List(addr), Symbol(eq), List(exp)] if op == "mem" && eq == ":=" => {
                Ok(Self::Store(Store {
                    address: Expr::parse(addr)?,
                    value: Expr::parse(exp)?,
                }))
            }
            [Symbol(op), List(addr), Symbol(eq), Symbol(byte), List(exp)]
                if op == "mem" && eq == ":=" && byte == "byte" =>
            {
                Ok(Self::StoreBits(StoreBits {
                    address: Expr::parse(addr)?,
                    value: Expr::parse(exp)?,
                    size: MemOpBytes::Byte,
                }))
            }

            // Shared stores
            [Symbol(op), Symbol(size), List(addr), List(exp)]
                if op == "shared_mem_store" && size == "word" =>
            {
                Ok(Self::SharedStore(SharedStore {
                    address: Expr::parse(addr)?,
                    value: Expr::parse(exp)?,
                }))
            }

            [Symbol(op), Symbol(size), List(addr), List(exp)]
                if op == "shared_mem_store" && size == "byte" =>
            {
                Ok(Self::SharedStoreBits(SharedStoreBits {
                    address: Expr::parse(addr)?,
                    value: Expr::parse(exp)?,
                    size: MemOpBytes::Byte,
                }))
            }

            [Symbol(op), Symbol(size), List(addr), List(exp)]
                if op == "shared_mem_store" && size == "halfword" =>
            {
                Ok(Self::SharedStoreBits(SharedStoreBits {
                    address: Expr::parse(addr)?,
                    value: Expr::parse(exp)?,
                    size: MemOpBytes::Byte,
                }))
            }

            // Shared loads
            [Symbol(op), Symbol(size), Symbol(dst), List(exp)]
                if op == "shared_mem_load" && size == "word" =>
            {
                Ok(Self::SharedLoad(SharedLoad {
                    address: Expr::parse(exp)?,
                    dst: Expr::Var(dst.into()),
                }))
            }

            [Symbol(op), Symbol(size), Symbol(dst), List(exp)]
                if op == "shared_mem_load" && size == "byte" =>
            {
                Ok(Self::SharedLoadBits(SharedLoadBits {
                    address: Expr::parse(exp)?,
                    dst: Expr::Var(dst.into()),
                    size: MemOpBytes::Byte,
                }))
            }

            [Symbol(op), Symbol(size), Symbol(dst), List(exp)]
                if op == "shared_mem_load" && size == "halfword" =>
            {
                Ok(Self::SharedLoadBits(SharedLoadBits {
                    address: Expr::parse(exp)?,
                    dst: Expr::Var(dst.into()),
                    size: MemOpBytes::HalfWord,
                }))
            }

            [Symbol(op), stmts @ ..] if op == "seq" => Self::parse_seq(stmts),

            // if
            [Symbol(op), List(cond), List(b1), List(b2)] if op == "if" => Ok(Self::If(If {
                cond: Expr::parse(cond)?,
                if_branch: Box::new(Self::parse(b1.iter().collect::<Vec<_>>())?),
                else_branch: Box::new(Self::parse(b2.iter().collect::<Vec<_>>())?),
            })),
            [Symbol(op), List(cond), List(b1), Symbol(b2)] if op == "if" => Ok(Self::If(If {
                cond: Expr::parse(cond)?,
                if_branch: Box::new(Self::parse(b1.iter().collect::<Vec<_>>())?),
                else_branch: Box::new(Self::parse_symbol(b2)?),
            })),
            [Symbol(op), List(cond), Symbol(b1), List(b2)] if op == "if" => Ok(Self::If(If {
                cond: Expr::parse(cond)?,
                if_branch: Box::new(Self::parse_symbol(b1)?),
                else_branch: Box::new(Self::parse(b2.iter().collect::<Vec<_>>())?),
            })),
            [Symbol(op), List(cond), Symbol(b1), Symbol(b2)] if op == "if" => Ok(Self::If(If {
                cond: Expr::parse(cond)?,
                if_branch: Box::new(Self::parse_symbol(b1)?),
                else_branch: Box::new(Self::parse_symbol(b2)?),
            })),
            // while
            [Symbol(op), List(cond), List(body)] if op == "while" => Ok(Self::While(While {
                cond: Expr::parse(cond)?,
                body: Box::new(Self::parse(body.iter().collect::<Vec<_>>())?),
            })),
            [Symbol(op), List(cond), Symbol(body)] if op == "while" => Ok(Self::While(While {
                cond: Expr::parse(cond)?,
                body: Box::new(Self::parse_symbol(body)?),
            })),
            [Symbol(op), List(label), List(args), Symbol(ret)] if op == "call" => {
                Ok(Self::Call(Call {
                    rettype: ret.into(),
                    fname: Expr::parse(label)?,
                    args: Expr::parse_slice(args)?,
                }))
            }
            [Symbol(op), List(label), List(args), Int(ret)] if op == "call" => {
                Ok(Self::Call(Call {
                    rettype: "todo_call".into(),
                    fname: Expr::parse(label)?,
                    args: Expr::parse_slice(args)?,
                }))
            }
            [Symbol(op), List(exp)] if op == "return" => Ok(Self::Return(Return {
                value: Expr::parse(exp)?,
            })),
            [Symbol(op), Symbol(name), List(arg0), List(arg1), List(arg2), List(arg3)]
                if op == "ext_call" =>
            {
                Ok(Self::ExtCall(ExtCall {
                    fname: name.clone(),
                    args: [
                        Expr::parse(arg0)?,
                        Expr::parse(arg1)?,
                        Expr::parse(arg2)?,
                        Expr::parse(arg3)?,
                    ],
                }))
            }
            [Symbol(op), List(label), List(args)] if op == "tail_call" => {
                Ok(Self::TailCall(TailCall {
                    fname: Expr::parse(label)?,
                    args: Expr::parse_slice(args)?,
                }))
            }
            [List(stmt)] => Self::parse(stmt.iter().collect::<Vec<_>>()),
            x => panic!("Could not parse stmt: {:?}", x),
        }
    }

    fn parse_dec(decl: Vec<&SExpr>, scope: Option<Vec<&SExpr>>) -> anyhow::Result<Self> {
        let scope = match scope {
            Some(stmts) => Self::parse(stmts)?,
            None => Self::Skip,
        };
        match &decl[..] {
            [Symbol(var), Symbol(eq), List(exp)] if eq == ":=" => {
                Ok(Self::Declaration(Declaration {
                    lhs: var.clone(),
                    rhs: Expr::parse(exp)?,
                    scope: Box::new(scope),
                }))
            }
            _ => Err(anyhow!("Not a valid declaration")),
        }
    }

    fn parse_seq(s: &[&SExpr]) -> anyhow::Result<Self> {
        if let [List(pos), pstmt] = s {
            match &pos[..] {
                [Symbol(annot), SString(loc), SString(position)]
                    if annot == "annot" && loc == "location" =>
                {
                    // TODO: return wrapper
                    return Self::parse(vec![pstmt]);
                }
                _ => (),
            }
        }
        let mut stmts = vec![];
        for stmt in s {
            match stmt {
                List(stmt) => stmts.push(Self::parse(stmt.iter().collect())?),
                Symbol(stmt) => stmts.push(Self::parse_symbol(stmt)?),
                _ => return Err(anyhow!("Error whilst parsing sequence")),
            }
        }
        Ok(Self::Seq(Seq { stmts }))
    }

    fn parse_symbol(symbol: &str) -> anyhow::Result<Self> {
        match symbol {
            "break" => Ok(Self::Break),
            "continue" => Ok(Self::Continue),
            "skip" => Ok(Self::Skip),
            "tick" => Ok(Self::Tick),
            x => Err(anyhow!("Failed to parse stmt symbol: {}", x)),
        }
    }
}

impl Arg {
    fn parse(s: &SExpr) -> anyhow::Result<Self> {
        match s {
            List(args) => match &args[..] {
                [Symbol(name), Symbol(colon), Symbol(shape)] if colon == ":" => Ok(Self {
                    name: name.clone(),
                    shape: Shape::parse(shape)?,
                }),
                [Symbol(name), Symbol(colon), Int(_)] if colon == ":" => Ok(Self {
                    name: name.clone(),
                    shape: Shape::Simple,
                }),
                _ => Err(anyhow!("Could not parse argument")),
            },
            _ => Err(anyhow!("Could not parse argument")),
        }
    }
}

impl FnDec {
    pub fn parse(s: SExpr) -> anyhow::Result<Self> {
        match s {
            List(l) => match &l[..] {
                [Symbol(fun_dec), Symbol(name), List(args), List(body)] if fun_dec == "func" => {
                    let args = args.iter().map(Arg::parse).collect::<anyhow::Result<_>>()?;
                    Ok(Self {
                        fname: name.clone(),
                        args,
                        body: Stmt::parse(body.iter().collect())?,
                    })
                }
                _ => Err(anyhow!("Shape of SExpr::List does not match")),
            },
            _ => Err(anyhow!("SExpr is not a list")),
        }
    }
}

impl Shape {
    pub fn parse(s: &str) -> anyhow::Result<Self> {
        if s == "1" {
            return Ok(Shape::Simple);
        }
        let mut stack = vec![];
        for c in s.chars() {
            match c {
                '<' => stack.push(vec![]),
                '1' => stack
                    .last_mut()
                    .ok_or(anyhow!("Unexpected symbol '1'"))?
                    .push(Shape::Simple),
                ',' => (),
                '>' => {
                    let item = stack.pop().ok_or(anyhow!("Unexpected symbol '>'"))?;
                    let shape = Shape::Nested(item);
                    if let Some(last) = stack.last_mut() {
                        last.push(shape);
                    } else {
                        stack.push(vec![shape]);
                    }
                }
                x => return Err(anyhow!("Unexpected symbol '{}'", x)),
            }
        }
        if stack.len() == 1 {
            let item = stack.pop().unwrap();
            match &item[..] {
                [shape] => Ok(shape.clone()),
                _ => Ok(Shape::Nested(item)),
            }
        } else {
            Err(anyhow!("Mismatched brackets"))
        }
    }
}
