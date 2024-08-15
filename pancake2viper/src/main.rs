mod pancake_ast;
mod parser;
mod translation;
mod viper_ast;

use anyhow::anyhow;
use pancake_ast::parse_fn_dec;
use parser::{get_sexprs, SExprParser};
use sexpr_parser::Parser;
use std::{env, fs};
use translation::translate_fndec;

fn main() -> anyhow::Result<()> {
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
    let vpr_prelude = fs::read_to_string("src/prelude.vpr")?;
    println!("{}\n", vpr_prelude);
    for ast in asts {
        println!("{}", translate_fndec(&ast));
    }

    Ok(())
}
