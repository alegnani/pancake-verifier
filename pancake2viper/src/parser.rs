use anyhow::anyhow;

pub fn get_sexprs_from_file(file_name: &str, cake_path: &str) -> anyhow::Result<Vec<String>> {
    let lines = fs::read_to_string(file_name)?;
    get_sexprs(lines, cake_path)
}

pub fn get_sexprs(lines: String, cake_path: &str) -> anyhow::Result<Vec<String>> {
    let mut preprocess = Command::new("cpp")
        .arg("-P")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let explore = Command::new(cake_path)
        .arg("--pancake")
        .arg("--explore")
        .stdin(Stdio::from(
            preprocess.stdout.ok_or(anyhow!("Could not pipe command"))?,
        ))
        .stdout(Stdio::piped())
        .spawn()?;

    let mut stdin = preprocess
        .stdin
        .take()
        .ok_or(anyhow!("Could not take stdin"))?;

    std::thread::spawn(move || stdin.write_all(lines.as_bytes()));

    let output_str = String::from_utf8(explore.wait_with_output()?.stdout)?;

    let pancake_lines = output_str
        .split('\n')
        .skip(2)
        .take_while(|l| !l.starts_with('#'))
        .collect::<Vec<_>>()
        .join("\n");

    Ok(pancake_lines
        .split("\n\n")
        .map(|s| s.to_owned())
        .collect::<Vec<_>>())
}

use std::{
    fs,
    io::Write,
    process::{Command, Stdio},
};

use sexpr_parser::{Parser, SexprFactory};

use crate::pancake::{self, parse_fn_dec};

/// Your amazing S-expression data structure
#[derive(Debug, PartialEq)]
pub enum SExpr {
    Int(u64),
    Float(f64),
    Symbol(String),
    SString(String),
    Pair(Box<(SExpr, SExpr)>),
    List(Vec<SExpr>),
}

pub struct SExprParser;

impl SExprParser {
    pub fn parse_function(sexpr: &str) -> anyhow::Result<pancake::FnDec> {
        parse_fn_dec(
            Self.parse(sexpr)
                .map_err(|_| anyhow!("Could not parse string to sexpr"))?,
        )
    }

    pub fn parse_program(sexprs: Vec<String>) -> anyhow::Result<pancake::Program> {
        let functions = sexprs
            .iter()
            .map(|s| Self::parse_function(s))
            .collect::<anyhow::Result<_>>()?;
        Ok(pancake::Program { functions })
    }
}

impl SexprFactory for SExprParser {
    type Sexpr = SExpr;
    type Integer = i64;
    type Float = f64;

    fn int(&mut self, x: i64) -> Self::Sexpr {
        SExpr::Int(x as u64)
    }

    fn float(&mut self, x: f64) -> Self::Sexpr {
        SExpr::Float(x)
    }

    fn symbol(&mut self, x: &str) -> Self::Sexpr {
        SExpr::Symbol(x.to_string())
    }

    fn string(&mut self, x: &str) -> Self::Sexpr {
        SExpr::SString(x.to_string())
    }

    fn list(&mut self, x: Vec<Self::Sexpr>) -> Self::Sexpr {
        SExpr::List(x)
    }

    fn pair(&mut self, a: Self::Sexpr, b: Self::Sexpr) -> Self::Sexpr {
        SExpr::Pair(Box::new((a, b)))
    }
}
