use std::str::FromStr;

use anyhow::anyhow;
use regex::Regex;
use viper::{AstFactory, Expr, LocalVarDecl, Type, Viper};

use crate::{ir::Program, ir_to_viper::EncodeOptions, pancake, ProgramToViper};

pub fn pretty_print(viper: &Viper, program: pancake::Program) -> anyhow::Result<String> {
    let vctx = viper.attach_current_thread();
    let utils = vctx.new_ast_utils();
    let ast = vctx.new_ast_factory();
    let ir: Program = program.into();
    Ok(utils.pretty_print(ir.to_viper(ast, EncodeOptions::default())))
}

pub trait ViperUtils<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>);
}

impl<'a> ViperUtils<'a> for AstFactory<'a> {
    fn new_var(&self, name: &str, typ: Type) -> (LocalVarDecl<'a>, Expr<'a>) {
        (self.local_var_decl(name, typ), self.local_var(name, typ))
    }
}

pub struct Position {
    begin: (i32, i32),
    end: (i32, i32),
}

impl FromStr for Position {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"\((\d+):(\d+) (\d+):(\d+)\)")?;
        if let Some(caps) = re.captures(s) {
            let v = caps
                .extract::<4>()
                .1
                .iter()
                .map(|n| n.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Self {
                begin: (v[0], v[1]),
                end: (v[2], v[3]),
            });
        }
        Err(anyhow!("Failed to parse Position, got {}", s))
    }
}

pub struct AstWrapper<T> {
    inner: T,
    position: Position,
}

impl<T> AstWrapper<T> {
    pub fn new(inner: T, pos: &str) -> anyhow::Result<Self> {
        Ok(Self {
            inner,
            position: Position::from_str(pos)?,
        })
    }
}
