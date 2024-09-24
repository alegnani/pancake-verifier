use crate::annotation;

use super::{top::ToViperType, ToViper, ViperEncodeCtx};

// impl<'a> ToViper<'a, viper::> for Annotation {
//     fn to_viper(self, ctx: &mut super::ViperEncodeCtx<'a>) -> T {
//         let ast = ctx.ast;
//         ast.
//         match self.typ {
//             Self::
//         };
//     }
// }

impl<'a> ToViperType<'a> for annotation::Type {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        let ast = ctx.ast;
        match self {
            Self::Bool => ast.bool_type(),
            Self::Int => ast.int_type(),
            Self::IArray => ctx.iarray.get_type(),
        }
    }
}

// impl<'a> ToViper<'a, viper::Expr<'a>> for annotation::Expr {
//     fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> viper::Expr<'a> {
//         match self {
//             S
//         }
//     }
// }
