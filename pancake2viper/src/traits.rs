use viper::AstFactory;

use crate::{
    ir_to_viper::{EncodeOptions, ViperEncodeCtx},
    shape::Shape,
};

pub trait ToViper<'a> {
    type Output;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output;
    fn to_viper_with_pos(
        &self,
        ctx: &mut ViperEncodeCtx<'a>,
        pos: viper::Position,
    ) -> Self::Output {
        todo!()
    }
}
pub trait ToViperType<'a> {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a>;
}

pub trait ToShape<'a> {
    fn shape(&self, ctx: &ViperEncodeCtx<'a>) -> Shape;
}

pub trait ProgramToViper<'a> {
    fn to_viper(self, ast: AstFactory<'a>, options: EncodeOptions) -> viper::Program<'a>;
}
