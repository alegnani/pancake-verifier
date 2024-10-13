use crate::{ir_to_viper::ViperEncodeCtx, shape::Shape, ShapeError, ToShape, TryToShape};

use super::{
    Arg, Expr, Field, FnDec, Function, Method, Predicate, Program, Stmt, Struct, TailCall,
};

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

impl Predicate {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Function {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Method {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl Expr {
    // TODO: this could well be a function pointer. If we stick to only using
    // valid function addresses (no unholy pointer arithmetic) we can encode
    // this as a switch statement checking the expression against all possible
    // function addresses.
    pub fn get_label(&self) -> String {
        match self {
            Self::Label(label) => label.to_owned(),
            _ => panic!("Probably using f-pointer"),
        }
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
            x @ (Stmt::Return(_) | Stmt::TailCall(_)) => {
                let shape = match x {
                    Stmt::Return(ret) => ret.value.to_shape(ctx),
                    Stmt::TailCall(tail) => tail.to_shape(ctx),
                    _ => unreachable!(),
                };
                match shape {
                    Ok(s) => Ok(vec![s]),
                    Err(ShapeError::UnknownReturnType(_)) => Ok(vec![]),
                    Err(e) => Err(e),
                }
            }
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
                Expr::Call(call) => ctx.get_function_type(&call.fname.get_label())?,
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

impl<'a> TryToShape<'a> for TailCall {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError> {
        ctx.get_function_type(&self.fname.get_label())
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
