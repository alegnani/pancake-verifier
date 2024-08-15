use anyhow::anyhow;

pub fn get_sexprs(file_name: &str) -> anyhow::Result<Vec<String>> {
    let cat = Command::new("cat")
        .arg(file_name)
        .stdout(Stdio::piped())
        .spawn()?;
    let cat_stdout = cat.stdout.ok_or(anyhow!("No output from cat"))?;
    let explore = Command::new("cake")
        .arg("--pancake")
        .arg("--explore")
        .stdin(Stdio::from(cat_stdout))
        .stdout(Stdio::piped())
        .spawn()?;
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

use std::process::{Command, Stdio};

use sexpr_parser::SexprFactory;

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
