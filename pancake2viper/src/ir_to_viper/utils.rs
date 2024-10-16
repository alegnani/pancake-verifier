use viper::Expr;

use crate::{
    ir::{self, Arg},
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
        match &self.typ {
            ir::Type::Struct(inner) => {
                let arg_var = ctx.ast.new_var(&self.name, self.typ.to_viper_type(ctx)).1;
                let len: usize = inner.iter().map(Shape::len).sum();
                let length = ast.int_lit(len as i64);
                let access_perm =
                    ctx.iarray
                        .array_acc_expr(arg_var, ast.int_lit(0), length, ast.full_perm());
                let length_pre = ast.eq_cmp(ctx.iarray.len_f(arg_var), length);
                Some(ast.and(length_pre, access_perm))
            }
            _ => None,
        }
    }
}

impl<'a> ToViperType<'a> for ir::Type {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        let ast = ctx.ast;
        match self {
            ir::Type::Bool => ast.bool_type(),
            ir::Type::Int => ast.int_type(),
            ir::Type::Array | ir::Type::Struct(_) => ctx.iarray.get_type(),
            x => panic!("Want type of {:?}", x),
        }
    }
}
