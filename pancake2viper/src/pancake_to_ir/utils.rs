use crate::{ir, pancake};

pub struct Wrapper<T>(pub Vec<T>);

impl From<Vec<pancake::Expr>> for Wrapper<ir::Expr> {
    fn from(value: Vec<pancake::Expr>) -> Self {
        Self(value.into_iter().map(|v| v.into()).collect())
    }
}

impl From<Vec<pancake::Stmt>> for Wrapper<ir::Stmt> {
    fn from(value: Vec<pancake::Stmt>) -> Self {
        Self(value.into_iter().map(|v| v.into()).collect())
    }
}

impl From<Vec<pancake::Arg>> for Wrapper<ir::Arg> {
    fn from(value: Vec<pancake::Arg>) -> Self {
        Self(value.into_iter().map(|v| v.into()).collect())
    }
}

impl From<Vec<pancake::FnDec>> for Wrapper<ir::FnDec> {
    fn from(value: Vec<pancake::FnDec>) -> Self {
        Self(value.into_iter().map(|v| v.into()).collect())
    }
}
