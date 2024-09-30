mod auto;
mod context;
mod mangler;

pub use context::*;
pub use mangler::*;

use crate::ToViper;

impl<'a, T: ToViper<'a>> ToViper<'a> for Vec<T> {
    type Output = Vec<T::Output>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        self.into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Vec<_>>()
    }
}

impl<'a, const T: usize, S: ToViper<'a>> ToViper<'a> for [S; T] {
    type Output = [S::Output; T];
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        self.map(|e| e.to_viper(ctx))
    }
}
