use super::{parse_stmt, shape::Shape, Stmt};
use crate::{
    parser::SExpr::{self, *},
    translation::top::ToShape,
};
use anyhow::anyhow;

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
}

impl FnDec {
    pub fn parse(s: SExpr) -> anyhow::Result<Self> {
        match s {
            List(l) => match &l[..] {
                [Symbol(fun_dec), Symbol(name), List(args), List(body)] if fun_dec == "func" => {
                    let args = args.iter().map(parse_arg).collect::<anyhow::Result<_>>()?;
                    Ok(FnDec {
                        fname: name.clone(),
                        args,
                        body: parse_stmt(body.iter().collect())?,
                    })
                }
                _ => Err(anyhow!("Shape of SExpr::List does not match")),
            },
            _ => Err(anyhow!("SExpr is not a list")),
        }
    }

    fn collect_returns(body: &Stmt, ctx: &crate::translation::ViperEncodeCtx<'_>) -> Vec<Shape> {
        match body {
            Stmt::Seq(seqn) => seqn
                .stmts
                .iter()
                .flat_map(|s| Self::collect_returns(s, ctx))
                .collect(),
            Stmt::Declaration(dec) => Self::collect_returns(&dec.scope, ctx),
            Stmt::Return(ret) => vec![ret.value.shape(ctx)],
            // Stmt::TailCall(tail) => tail.shape(ctx), // TODO: evaluate return types
            _ => vec![],
        }
    }
}

impl<'a> ToShape<'a> for FnDec {
    fn shape(&self, ctx: &crate::translation::ViperEncodeCtx<'a>) -> Shape {
        let shapes = FnDec::collect_returns(&self.body, ctx);
        // TODO: add check for types to be the same and non-empty
        shapes[0].clone()
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub shape: Shape,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
}

fn parse_arg(s: &SExpr) -> anyhow::Result<Arg> {
    match s {
        List(args) => match &args[..] {
            [Symbol(name), Symbol(colon), Symbol(shape)] if colon == ":" => Ok(Arg {
                name: name.clone(),
                shape: Shape::parse(shape)?,
            }),
            [Symbol(name), Symbol(colon), Int(_)] if colon == ":" => Ok(Arg {
                name: name.clone(),
                shape: Shape::Simple,
            }),
            _ => Err(anyhow!("Could not parse argument")),
        },
        _ => Err(anyhow!("Could not parse argument")),
    }
}
