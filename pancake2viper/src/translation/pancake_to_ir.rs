use crate::{annotation::parse_annot, ir, pancake};

impl From<pancake::Annotation> for ir::Annotation {
    fn from(value: pancake::Annotation) -> Self {
        parse_annot(&value.line)
    }
}

impl From<pancake::Assign> for ir::Assign {
    fn from(value: pancake::Assign) -> Self {
        Self {
            lhs: value.lhs,
            rhs: value.rhs.into(),
        }
    }
}

impl From<pancake::Expr> for ir::Expr {
    fn from(value: pancake::Expr) -> Self {
        todo!()
    }
}

impl From<pancake::Stmt> for ir::Stmt {
    fn from(value: pancake::Stmt) -> Self {
        todo!()
    }
}
