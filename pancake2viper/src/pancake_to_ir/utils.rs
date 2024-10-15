use crate::utils::{ToIR, TryToIR};

impl<T: TryToIR> TryToIR for Vec<T> {
    type Output = Vec<T::Output>;

    fn to_ir(
        self,
        ctx: &mut crate::utils::TypeContext,
    ) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(|a| a.to_ir(ctx)).collect()
    }
}

impl<T: TryToIR, const N: usize> TryToIR for [T; N] {
    type Output = Vec<T::Output>;

    fn to_ir(
        self,
        ctx: &mut crate::utils::TypeContext,
    ) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(|a| a.to_ir(ctx)).collect()
    }
}
impl<T: ToIR> ToIR for Vec<T> {
    type Output = Vec<T::Output>;

    fn to_ir(self, ctx: &mut crate::utils::TypeContext) -> Self::Output {
        self.into_iter().map(|a| a.to_ir(ctx)).collect()
    }
}
