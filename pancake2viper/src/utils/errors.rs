use thiserror::Error;

use crate::{ir, pancake};

use super::shape::Shape;

#[derive(Error, Debug)]
pub enum ToViperError {
    #[error("Conversion to Shape failed")]
    ShapeError(#[from] ShapeError),
    #[error("Translation error")]
    TranslationError(#[from] TranslationError),
    #[error("Field access in annotation can't yield non `1` shape, got {0:?}")]
    FieldAccessChainShape(Shape),
    #[error("Condition must be of shape `1`, got {0:?}")]
    ConditionShape(Shape),
    #[error("Invalid fold/unfold statement: Expression should be predicate access, got {0:?}")]
    InvalidFold(ir::Expr),
    #[error("Assignment shape mismatch: Lhs: {0:?}, Rhs: {1:?}")]
    MismatchedShapes(Shape, Shape),
    #[error("Can't specify pre-/post-conditions in this position, consider moving it up")]
    InvalidAnnotation,
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
}

#[derive(Error, Debug)]
pub enum TranslationError {
    #[error("Mangling error")]
    MangleError(#[from] MangleError),
    #[error("Shape error")]
    ShapeError(#[from] ShapeError),
    #[error("Shape for '{0}' is not known")]
    UnknownShape(String),
    #[error("Function {0} has no return type set")]
    UnknownReturnType(String),
    #[error("Invalid label, might be function pointer. Got {0:?}")]
    InvalidLabel(pancake::Expr),
}

#[derive(Error, Debug)]
pub enum MangleError {
    #[error("'{0}' is a reserved keyword and can't be used as an identifier")]
    ReservedKeyword(String),
    #[error("Variable '{0}' has been declared twice")]
    DoubleDeclaration(String),
    #[error("Variable '{0}' has not been declared")]
    UndeclaredVar(String),
}
