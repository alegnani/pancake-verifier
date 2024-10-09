use thiserror::Error;
use viper::AstFactory;

use crate::{
    ir::{self, Annotation},
    ir_to_viper::{EncodeOptions, ViperEncodeCtx},
    shape::Shape,
};

#[derive(Error, Debug)]
pub enum ToViperError {
    #[error("Conversion to Shape failed")]
    ShapeError(#[from] ShapeError),
    #[error("Field access in annotation can't yield non `1` shape, got {0:?}")]
    FieldAccessChainShape(Shape),
    #[error("Condition must be of shape `1`, got {0:?}")]
    ConditionShape(Shape),
    #[error("Invalid fold/unfold statement: Expression should be predicate access, got {0:?}")]
    InvalidFold(ir::Expr),
    #[error("Assignment shape mismatch: Lhs: {0:?}, Rhs: {1:?}")]
    MismatchedShapes(Shape, Shape),
    #[error("Only preconditions and postconditions can be specified, got {0:?}")]
    InvalidAnnotation(Vec<Annotation>),
}

pub trait TryToViper<'a> {
    type Output;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError>;
}

pub trait ToViper<'a> {
    type Output;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output;
    // fn to_viper_with_pos(
    //     &self,
    //     ctx: &mut ViperEncodeCtx<'a>,
    //     pos: viper::Position,
    // ) -> Self::Output {
    //     todo!()
    // }
}
pub trait ToViperType<'a> {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a>;
}

pub trait ToShape<'a> {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape;
}

#[derive(Error, Debug)]
pub enum ShapeError {
    #[error("Invalid field access: {0:?} is of shape `1`")]
    SimpleShapeFieldAccess(ir::Expr),
    #[error("Invalid access: {0:?} is of shape `1`")]
    SimpleShapeAccess(Shape),
    #[error("Field access with index {0} out of bounds for struct of shape `{1:?}`")]
    OutOfBoundsFieldAccess(usize, Shape),
}

pub trait TryToShape<'a> {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError>;
}

pub trait ProgramToViper<'a> {
    fn to_viper(
        self,
        ast: AstFactory<'a>,
        options: EncodeOptions,
    ) -> Result<viper::Program<'a>, ToViperError>;
}
