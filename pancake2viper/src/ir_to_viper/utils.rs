use viper::Expr;

use crate::{
    ir::Arg,
    utils::{Shape, ToViper, ToViperError, ToViperType, TryToViper, ViperEncodeCtx, ViperUtils},
};

impl<'a, T: TryToViper<'a>> TryToViper<'a> for Vec<T> {
    type Output = Vec<T::Output>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        self.into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()
    }
}

impl<'a, T: ToViper<'a>> ToViper<'a> for Vec<T> {
    type Output = Vec<T::Output>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        self.into_iter().map(|a| a.to_viper(ctx)).collect()
    }
}

impl Arg {
    /// Generates preconditions for an argument
    ///
    /// If an `Arg` is not of shape `1` it is encoded as an `IArray`.
    /// Therefore it needs both access permissions to the slots and its length
    /// as a precondition in the method.
    pub fn permission<'a>(&self, ctx: &ViperEncodeCtx<'a>) -> Option<Expr<'a>> {
        let ast = ctx.ast;
        match self.shape {
            Shape::Simple => None,
            Shape::Nested(_) => {
                let arg_var = ctx.ast.new_var(&self.name, self.shape.to_viper_type(ctx)).1;
                let length = ast.int_lit(self.shape.len() as i64);
                let access_perm =
                    ctx.iarray
                        .array_acc_expr(arg_var, ast.int_lit(0), length, ast.full_perm());
                let length_pre = ast.eq_cmp(ctx.iarray.len_f(arg_var), length);
                Some(ast.and(length_pre, access_perm))
            }
        }
    }
}