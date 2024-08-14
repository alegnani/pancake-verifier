// use viper;
mod pancake_ast;
mod translation;
mod viper_ast;

use anyhow::anyhow;
use pancake_ast::parse_fn_dec;
use std::{
    env,
    process::{Command, Stdio},
};
use translation::translate_fndec;

use sexpr_parser::{parse, Parser};

fn main() -> anyhow::Result<()> {
    //     // let v = viper::Viper::new("");
    //     // let vctx = v.attach_current_thread();
    //     // let ast_factory = vctx.new_ast_factory();
    //     // let t = ast_factory.int_type();
    //     // println!("Type: {}", t);
    let args = env::args().collect::<Vec<_>>();
    let mut asts = vec![];
    for fn_sexpr in get_sexprs(&args[1])? {
        println!("\n Function:\n");
        let s = SExprParser
            .parse(&fn_sexpr)
            .map_err(|_| anyhow!("Could not parse sexpr"))?;
        let ast = parse_fn_dec(s)?;
        println!("AST: {:?}", &ast.body);
        asts.push(ast);
    }

    println!("\n\n################# VIPER #################\n\n");
    for ast in asts {
        println!("{}", translate_fndec(&ast));
    }

    Ok(())
}

fn get_sexprs(file_name: &str) -> anyhow::Result<Vec<String>> {
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

use sexpr_parser::SexprFactory;

/// Your amazing S-expression data structure
#[derive(Debug, PartialEq)]
enum SExpr {
    Int(u64),
    Float(f64),
    Symbol(String),
    SString(String),
    Pair(Box<(SExpr, SExpr)>),
    List(Vec<SExpr>),
}

struct SExprParser;

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
