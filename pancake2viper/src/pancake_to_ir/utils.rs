use crate::{
    ir::{Annotation, AnnotationType, Definition, Expr, Seq, Stmt, Type},
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

fn stmt_annotation_helper(
    body: &mut Stmt,
    mut preposts: (Vec<Expr>, Vec<Expr>),
) -> (Vec<Expr>, Vec<Expr>) {
    match body {
        Stmt::Annotation(Annotation {
            typ: AnnotationType::Precondition,
            expr,
        }) => {
            preposts.0.push(expr.to_owned());
            *body = Stmt::Skip;
        }
        Stmt::Annotation(Annotation {
            typ: AnnotationType::Postcondition,
            expr,
        }) => {
            preposts.1.push(expr.to_owned());
            *body = Stmt::Skip;
        }
        Stmt::Seq(Seq { stmts }) => {
            for stmt in stmts {
                preposts = stmt_annotation_helper(stmt, preposts);
            }
        }
        Stmt::Definition(Definition {
            scope,
            lhs: _,
            rhs: _,
        }) => preposts = stmt_annotation_helper(scope, preposts),
        _ => (),
    }
    preposts
}

/// Finds pre- and post-conditions in the statement and returns them by removing them.
pub fn stmt_annotation_push(body: &mut Stmt) -> (Vec<Expr>, Vec<Expr>) {
    stmt_annotation_helper(body, (vec![], vec![]))
}
