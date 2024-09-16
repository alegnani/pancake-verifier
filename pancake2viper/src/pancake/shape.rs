use anyhow::anyhow;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Shape {
    Simple,
    Nested(Vec<Shape>),
}

impl Shape {
    pub fn len(&self) -> usize {
        match self {
            Self::Simple => 1,
            Self::Nested(l) => l.iter().map(|e| e.len()).sum(),
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

    pub fn access(&self, idx: usize) -> (usize, usize) {
        match self {
            Self::Simple => panic!("Can't acces field of shape '1'"),
            Self::Nested(elems) => {
                assert!(idx < elems.len());
                let size = elems[idx].len();
                let offset = elems.iter().take(idx).map(Self::len).sum();
                (offset, size)
            }
        }
    }

    pub fn parse(s: &str) -> anyhow::Result<Self> {
        if s == "1" {
            return Ok(Shape::Simple);
        }
        let mut stack = vec![];
        for c in s.chars() {
            match c {
                '<' => stack.push(vec![]),
                '1' => stack
                    .last_mut()
                    .ok_or(anyhow!("Unexpected symbol '1'"))?
                    .push(Shape::Simple),
                ',' => (),
                '>' => {
                    let item = stack.pop().ok_or(anyhow!("Unexpected symbol '>'"))?;
                    let shape = Shape::Nested(item);
                    if let Some(last) = stack.last_mut() {
                        last.push(shape);
                    } else {
                        stack.push(vec![shape]);
                    }
                    // let shape = match &item[..] {
                    //     [Self::Simple] => Self::Simple,
                    //     _ => Self::Nested(item),
                    // };
                    // match stack.last_mut() {
                    //     Some(last) => last.push(shape),
                    //     None => stack.push(vec![shape]),
                    // }
                }
                x => return Err(anyhow!("Unexpected symbol '{}'", x)),
            }
        }
        if stack.len() == 1 {
            let item = stack.pop().unwrap();
            match &item[..] {
                [shape] => Ok(shape.clone()),
                _ => Ok(Shape::Nested(item)),
            }
        } else {
            Err(anyhow!("Mismatched brackets"))
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
