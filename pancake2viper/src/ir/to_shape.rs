use crate::{
    ir,
    utils::{Mangler, Shape, ShapeError, ToShape, TranslationError, TryToShape, TypeContext},
};

impl TryToShape for ir::Struct {
    fn to_shape(&self, ctx: &TypeContext) -> Result<Shape, TranslationError> {
        let inner_shapes = self
            .elements
            .iter()
            .map(|e| e.to_shape(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Shape::Nested(inner_shapes))
    }
}

impl TryToShape for ir::Field {
    fn to_shape(&self, ctx: &TypeContext) -> Result<Shape, TranslationError> {
        let obj_shape = self.obj.to_shape(ctx)?;
        match &obj_shape {
            Shape::Simple => Err(ShapeError::IRSimpleShapeFieldAccess(*self.obj.clone()).into()),
            Shape::Nested(ls) => ls
                .get(self.field_idx)
                .cloned()
                .ok_or(ShapeError::OutOfBoundsFieldAccess(self.field_idx, obj_shape).into()),
        }
    }
}

impl ToShape for ir::Type {
    fn to_shape(&self, _ctx: &TypeContext) -> Shape {
        use ir::Type::*;
        match self {
            Bool | Int => Shape::Simple,
            IArray => Shape::Nested(vec![]),
        }
    }
}

impl TryToShape for ir::Expr {
    fn to_shape(&self, ctx: &TypeContext) -> Result<Shape, TranslationError> {
        use ir::Expr::*;
        match self {
            Field(field) => field.to_shape(ctx),
            Struct(struc) => struc.to_shape(ctx),
            UnfoldingIn(unfold) => unfold.expr.to_shape(ctx),
            MethodCall(call) => ctx.get_function_type(&call.fname),
            FunctionCall(call) => ctx.get_function_type(&call.fname),
            x => Ok(match x {
                Const(_) | UnOp(_) | BinOp(_) | Shift(_) | LoadByte(_) | Quantified(_)
                | ArrayAccess(_) | AccessPredicate(_) | FieldAccessChain(_) | BaseAddr
                | BytesInWord => Shape::Simple,
                Var(var) => ctx.get_type_no_mangle(var)?,
                Label(_) => unreachable!("ToShape for Expr::Label"),
                Load(load) => load.shape.clone(),
                _ => unreachable!(),
            }),
        }
    }
}