use crate::{
    ir::Type,
    utils::{Shape, ToType, TryToIR},
};

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

impl ToType for Shape {
    fn to_type(&self) -> Type {
        match self {
            Self::Simple => Type::Int,
            Self::Nested(inner) => Type::Struct(inner.clone()),
        }
    }
}
