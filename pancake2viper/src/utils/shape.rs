use super::{errors::ShapeError, traits::ToViperType, ViperEncodeCtx};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Shape {
    Simple,
    Nested(Vec<Self>),
}

impl Shape {
    pub fn len(&self) -> usize {
        match self {
            Self::Simple => 1,
            Self::Nested(l) => l.iter().map(Self::len).sum(),
        }
    }

    pub fn is_simple(&self) -> bool {
        match self {
            Self::Simple => true,
            Self::Nested(_) => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn access(&self, idx: usize) -> Result<(usize, usize), ShapeError> {
        match self {
            Self::Simple => Err(ShapeError::SimpleShapeAccess(self.clone())),
            Self::Nested(elems) => {
                assert!(idx < elems.len());
                let size = elems[idx].len();
                let offset = elems.iter().take(idx).map(Self::len).sum();
                Ok((offset, size))
            }
        }
    }
}

impl<'a> ToViperType<'a> for Shape {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        match self {
            Self::Simple => ctx.ast.int_type(),
            Self::Nested(_) => ctx.iarray.get_type(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Shape::{self, *};

    #[test]
    fn parse_simple() {
        let s = "1";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Shape::Simple);
        assert_eq!(shape.len(), 1);
    }

    #[test]
    fn parse_nested1() {
        let s = "<1>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Nested(vec![Simple]));
        assert_eq!(shape.len(), 1);
    }

    #[test]
    fn parse_nested2() {
        let s = "<1,<1,1>>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Nested(vec![Simple, Nested(vec![Simple, Simple])]));
        assert_eq!(shape.len(), 3);
    }

    #[test]
    fn parse_nested3() {
        let s = "<1,<1,1,<1>>,<1,1>,1>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(
            shape,
            Nested(vec![
                Simple,
                Nested(vec![Simple, Simple, Nested(vec![Simple])]),
                Nested(vec![Simple, Simple]),
                Simple
            ])
        );
        assert_eq!(shape.len(), 7);
    }
}
