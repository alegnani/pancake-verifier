pub mod context;
pub mod mangler;
pub mod pancake_to_ir;

pub use context::{EncodeOptions, ViperEncodeCtx};
use viper::AstFactory;

use crate::pancake::Shape;

pub trait ToViper<'a, T> {
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> T;
    fn to_viper_with_pos(&self, ctx: &mut ViperEncodeCtx<'a>, pos: viper::Position) -> T {
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
