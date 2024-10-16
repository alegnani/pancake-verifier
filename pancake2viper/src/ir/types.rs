use crate::{
    ir,
    utils::{Shape, ToShape, TranslationError, TryToShape, TypeContext, TypeResolution},
};

use super::{FnDec, Stmt};

impl TypeResolution for ir::Expr {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            ir::Expr::Quantified(quant) => quant.decls.resolve_type(ctx),
            _ => Ok(()),
        }
    }
}

impl TypeResolution for ir::Annotation {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.expr.resolve_type(ctx)
    }
}

impl TypeResolution for ir::Stmt {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            ir::Stmt::Definition(def) => {
                match def.rhs.to_shape(ctx) {
                    Ok(shape) => ctx.set_type(def.lhs.clone(), shape),
                    Err(TranslationError::UnknownReturnType(_))
                    | Err(TranslationError::UnknownShape(_)) => (), // TODO: do something
                    Err(e) => return Err(e),
                }
                def.scope.resolve_type(ctx)
            }
            ir::Stmt::If(i) => {
                let if_type = i.if_branch.resolve_type(ctx);
                ignore_unknown(if_type)?;
                i.else_branch.resolve_type(ctx)
            }
            ir::Stmt::Annotation(annot) => annot.resolve_type(ctx),
            ir::Stmt::While(w) => w.body.resolve_type(ctx),
            ir::Stmt::Seq(seq) => seq.stmts.resolve_type(ctx),
            _ => Ok(()),
        }
    }
}

impl<T: TypeResolution> TypeResolution for Vec<T> {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        for stmt in self {
            let typ = stmt.resolve_type(ctx);
            ignore_unknown(typ)?;
        }
        Ok(())
    }
}

impl<T: TypeResolution> TypeResolution for Option<T> {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            Some(t) => t.resolve_type(ctx),
            None => Ok(()),
        }
    }
}

impl TypeResolution for ir::Arg {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.shape.clone());
        Ok(())
    }
}

impl TypeResolution for ir::FnDec {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.body.resolve_type(ctx)?;
        let returns = Self::collect_returns(&self.body, ctx)?;
        if returns.is_empty() {
            Err(TranslationError::UnknownReturnType(self.fname.clone()))
        } else {
            ctx.set_type(self.fname.clone(), returns[0].clone());
            Ok(())
        }
    }
}

impl TypeResolution for ir::Decl {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.to_shape(ctx));
        Ok(())
    }
}

impl TypeResolution for ir::Predicate {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.body.resolve_type(ctx)?;
        ctx.set_type(self.name.clone(), Shape::Simple);
        Ok(())
    }
}

impl TypeResolution for ir::Function {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.body.resolve_type(ctx)?;
        self.preposts.resolve_type(ctx)?;
        ctx.set_type(self.name.clone(), self.typ.to_shape(ctx));
        Ok(())
    }
}

impl TypeResolution for ir::AbstractMethod {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.rettyps.resolve_type(ctx)?;
        assert!(self.rettyps.len() <= 1); // TODO: add support for multiple returns
        let shape = if self.rettyps.is_empty() {
            Shape::Simple
        } else {
            ctx.get_type_no_mangle(&self.rettyps[0].name).unwrap()
        };
        ctx.set_type(self.name.clone(), shape);
        Ok(())
    }
}

impl FnDec {
    pub fn collect_returns(body: &Stmt, ctx: &TypeContext) -> Result<Vec<Shape>, TranslationError> {
        match body {
            Stmt::Seq(seqn) => Ok(seqn
                .stmts
                .iter()
                .flat_map(|s| Self::collect_returns(s, ctx))
                .flatten()
                .collect()),
            Stmt::Definition(def) => Self::collect_returns(&def.scope, ctx),
            Stmt::Return(ret) => match ret.value.to_shape(ctx) {
                Ok(s) => Ok(vec![s]),
                Err(TranslationError::UnknownReturnType(_))
                | Err(TranslationError::UnknownShape(_)) => Ok(vec![]),
                Err(e) => Err(e),
            },
            _ => Ok(vec![]),
        }
    }
}

impl ir::Program {
    pub fn resolve_types(&self) -> Result<TypeContext, TranslationError> {
        let mut ctx = TypeContext::new();
        let mut prev_size = ctx.size();
        loop {
            ignore_unknown(self.viper_functions.resolve_type(&mut ctx))?;
            ignore_unknown(self.predicates.resolve_type(&mut ctx))?;
            ignore_unknown(self.methods.resolve_type(&mut ctx))?;
            ignore_unknown(self.functions.resolve_type(&mut ctx))?;
            let new_size = ctx.size();
            if new_size == prev_size {
                break;
            }
            prev_size = new_size;
        }
        Ok(ctx)
    }
}

fn ignore_unknown(result: Result<(), TranslationError>) -> Result<(), TranslationError> {
    match result {
        Ok(_)
        | Err(TranslationError::UnknownReturnType(_))
        | Err(TranslationError::UnknownShape(_)) => Ok(()),
        Err(x) => Err(x),
    }
}
