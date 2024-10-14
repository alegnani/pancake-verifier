use thiserror::Error;

use crate::{ir, pancake};

use super::shape::Shape;

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
    InvalidAnnotation(Vec<ir::Annotation>),
}

#[derive(Error, Debug)]
pub enum ShapeError {
    #[error("Invalid field access: {0:?} is of shape `1`")]
    PancakeSimpleShapeFieldAccess(pancake::Expr),
    #[error("Invalid field access: {0:?} is of shape `1`")]
    IRSimpleShapeFieldAccess(ir::Expr),
    #[error("Invalid access: {0:?} is of shape `1`")]
    SimpleShapeAccess(Shape),
    #[error("Field access with index {0} out of bounds for struct of shape `{1:?}`")]
    OutOfBoundsFieldAccess(usize, Shape),
    #[error("Function {0} has no return type set")]
    UnknownReturnType(String),
}

#[derive(Error, Debug)]
pub enum ToIRError {}
