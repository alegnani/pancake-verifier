use viper::{AstFactory, Expr, LocalVarDecl};

use crate::ir::{self, Type};

use super::{
    errors::ToViperError, shape::Shape, EncodeOptions, Mangler, TranslationError, TypeContext,
    ViperEncodeCtx,
};

pub trait TryToIR {
    type Output;
    fn to_ir(self) -> Result<Self::Output, TranslationError>;
}

pub trait TryToIRGeneric<T> {
    fn to_ir(self) -> Result<T, TranslationError>;
}

pub trait Mangleable {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError>;
}

pub trait TypeResolution {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError>;
}

pub trait ExprTypeResolution {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError>;
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

pub trait ToShape {
    fn to_shape(&self, ctx: &TypeContext) -> Shape;
}

pub trait TryToShape {
    fn to_shape(&self, ctx: &TypeContext) -> Result<Shape, TranslationError>;
}

pub trait TryToType {
    fn to_type(&self, ctx: &TypeContext) -> Result<ir::Type, TranslationError>;
}

pub trait ToType {
    fn to_type(&self) -> ir::Type;
}

pub trait ConstEvalExpr {
    fn const_eval(self, options: &EncodeOptions) -> ir::Expr;
}

pub trait ConstEval {
    fn const_eval(self, options: &EncodeOptions) -> Self;
}

pub trait ExprSubstitution {
    fn substitute(&mut self, old: &ir::Expr, new: &ir::Expr) -> bool;
}

pub trait ProgramToViper<'a> {
    fn to_viper(
        self,
        types: TypeContext,
        ast: AstFactory<'a>,
        options: EncodeOptions,
    ) -> Result<viper::Program<'a>, ToViperError>;
}

pub trait ViperUtils<'a> {
    fn new_var(&self, name: &str, typ: viper::Type) -> (LocalVarDecl<'a>, Expr<'a>);
    fn zero(&self) -> Expr<'a>;
    fn one(&self) -> Expr<'a>;
    fn two(&self) -> Expr<'a>;
}
