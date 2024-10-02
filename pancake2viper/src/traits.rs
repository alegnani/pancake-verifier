use thiserror::Error;
use viper::AstFactory;

use crate::{
    ir,
    ir_to_viper::{EncodeOptions, ViperEncodeCtx},
    shape::Shape,
};

#[derive(Error, Debug)]
pub enum ToViperError {
    #[error("Conversion to Shape failed")]
    ShapeError(#[from] ShapeError),
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
    #[error("Field access with index {0} out of bounds for struct of shape `{1:?}`")]
    OutOfBoundsFieldAccess(usize, Shape),
}

pub trait TryToShape<'a> {
    fn to_shape(&self, ctx: &ViperEncodeCtx<'a>) -> Result<Shape, ShapeError>;
}

pub trait ProgramToViper<'a> {
    fn to_viper(self, ast: AstFactory<'a>, options: EncodeOptions) -> viper::Program<'a>;
}
