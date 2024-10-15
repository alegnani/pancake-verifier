use crate::utils::{Mangler, TryToIR};

impl<T: TryToIR> TryToIR for Vec<T> {
    type Output = Vec<T::Output>;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(|a| a.to_ir(mangler)).collect()
    }
}

impl<T: TryToIR, const N: usize> TryToIR for [T; N] {
    type Output = Vec<T::Output>;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, crate::utils::TranslationError> {
        self.into_iter().map(|a| a.to_ir(mangler)).collect()
    }
}
