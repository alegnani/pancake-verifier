use crate::translation::ToShape;

use super::{Expr, FnDec, Shape, Stmt, Struct};

impl Struct {
    pub fn new(elements: Vec<Expr>) -> Self {
        Self { elements }
    }

    pub fn flatten(&self) -> Vec<Expr> {
        self.elements
            .iter()
            .flat_map(|e| match e {
                Expr::Struct(inner) => inner.flatten(),
                x => vec![x.clone()],
            })
            .collect()
    }
}

impl FnDec {
    pub fn collect_returns(
        body: &Stmt,
        ctx: &crate::translation::ViperEncodeCtx<'_>,
    ) -> Vec<Shape> {
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
