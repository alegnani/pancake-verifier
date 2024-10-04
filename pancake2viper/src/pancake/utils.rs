use crate::{ir_to_viper::ViperEncodeCtx, shape::Shape, ShapeError, ToShape, TryToShape};

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
    pub fn collect_returns(
        body: &Stmt,
        ctx: &ViperEncodeCtx<'_>,
    ) -> Result<Vec<Shape>, ShapeError> {
        match body {
            Stmt::Seq(seqn) => Ok(seqn
                .stmts
                .iter()
                .flat_map(|s| Self::collect_returns(s, ctx))
                .flatten()
                .collect()),
            Stmt::Declaration(dec) => Self::collect_returns(&dec.scope, ctx),
            Stmt::Return(ret) => Ok(vec![ret.value.to_shape(ctx)?]),
            // Stmt::TailCall(tail) => tail.shape(ctx), // TODO: evaluate return types
            _ => Ok(vec![]),
        }
    }
}

impl<'a> TryToShape<'a> for Expr {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError> {
        match self {
            Expr::Struct(struc) => struc.to_shape(ctx),
            Expr::Field(field) => field.to_shape(ctx),
            x => Ok(match x {
                Expr::Const(_)
                | Expr::Op(_)
                | Expr::Shift(_)
                | Expr::LoadByte(_)
                | Expr::BaseAddr
                | Expr::BytesInWord => Shape::Simple,
                Expr::Var(var) => ctx.get_type(var),
                Expr::Call(call) => Shape::Simple, // FIXME
                Expr::Label(_) => unreachable!("ToShape for Expr::Label"),
                Expr::Load(load) => load.shape.clone(),
                _ => unreachable!(),
            }),
        }
    }
}

impl<'a> TryToShape<'a> for Struct {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError> {
        let inner_shapes = self
            .elements
            .iter()
            .map(|e| e.to_shape(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Shape::Nested(inner_shapes))
    }
}

impl<'a> TryToShape<'a> for Field {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError> {
        let obj_shape = self.obj.to_shape(ctx)?;
        match &obj_shape {
            Shape::Simple => Err(ShapeError::SimpleShapeFieldAccess(
                (*self.obj).clone().into(),
            )),
            Shape::Nested(ls) => {
                ls.get(self.field_idx)
                    .cloned()
                    .ok_or(ShapeError::OutOfBoundsFieldAccess(
                        self.field_idx,
                        obj_shape,
                    ))
            }
        }
    }
}

impl<'a> ToShape<'a> for Arg {
    fn to_shape(&self, _ctx: &ViperEncodeCtx<'a>) -> Shape {
        self.shape.clone()
    }
}

impl<'a> TryToShape<'a> for FnDec {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError> {
        let shapes = FnDec::collect_returns(&self.body, ctx)?;
        // TODO: add check for types to be the same and non-empty
        Ok(shapes[0].clone())
    }
}
