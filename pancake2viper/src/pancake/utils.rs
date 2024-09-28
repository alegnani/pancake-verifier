use crate::{ir_to_viper::ViperEncodeCtx, shape::Shape, ToShape};

use super::{Arg, Expr, Field, FnDec, Stmt, Struct};

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
    pub fn collect_returns(body: &Stmt, ctx: &ViperEncodeCtx<'_>) -> Vec<Shape> {
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

impl<'a> ToShape<'a> for Expr {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        match self {
            Expr::Const(_)
            | Expr::Op(_)
            | Expr::Shift(_)
            | Expr::LoadByte(_)
            | Expr::BaseAddr
            | Expr::BytesInWord => Shape::Simple,
            Expr::Var(var) => ctx.get_type(var),
            Expr::Call(call) => Shape::Simple, // FIXME
            Expr::Label(_) => panic!("Should not be possible"),
            Expr::Load(load) => load.shape.clone(),
            Expr::Field(field) => field.shape(ctx),
            Expr::Struct(struc) => struc.shape(ctx),
        }
    }
}

impl<'a> ToShape<'a> for Struct {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let inner_shapes = self
            .elements
            .iter()
            .map(|e| e.shape(ctx))
            .collect::<Vec<_>>();
        Shape::Nested(inner_shapes)
    }
}

impl<'a> ToShape<'a> for Field {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let obj_shape = self.obj.shape(ctx);
        match obj_shape {
            Shape::Simple => panic!("Field access into value of shape '1'"),
            Shape::Nested(ls) => {
                assert!(self.field_idx < ls.len(), "Field access out of bounds");
                ls[self.field_idx].clone()
            }
        }
    }
}

impl<'a> ToShape<'a> for Arg {
    fn shape(&self, _ctx: &ViperEncodeCtx<'a>) -> Shape {
        self.shape.clone()
    }
}

impl<'a> ToShape<'a> for FnDec {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape {
        let shapes = FnDec::collect_returns(&self.body, ctx);
        // TODO: add check for types to be the same and non-empty
        shapes[0].clone()
    }
}
