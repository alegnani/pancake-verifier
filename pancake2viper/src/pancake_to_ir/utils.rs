use crate::utils::TryToIR;

impl<T: TryToIR> TryToIR for Vec<T> {
    type Output = Vec<T::Output>;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(TryToIR::to_ir).collect()
    }
}

impl<T: TryToIR, const N: usize> TryToIR for [T; N] {
    type Output = Vec<T::Output>;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(TryToIR::to_ir).collect()
    }
}
