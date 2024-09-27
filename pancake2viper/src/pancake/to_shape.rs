use crate::translation::{ToShape, ViperEncodeCtx};

use super::*;

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
