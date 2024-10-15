use std::fmt::Debug;

use crate::{
    ir,
    utils::{Shape, TranslationError, TryToShape, TypeContext, TypeResolution},
};

use super::{FnDec, Stmt};

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
                match if_type {
                    Ok(_)
                    | Err(TranslationError::UnknownReturnType(_))
                    | Err(TranslationError::UnknownShape(_)) => (),
                    x => return x,
                }
                i.else_branch.resolve_type(ctx)
            }
            ir::Stmt::While(w) => w.body.resolve_type(ctx),
            ir::Stmt::Seq(seq) => seq.stmts.resolve_type(ctx),
            _ => Ok(()),
        }
    }
}

impl<T: TypeResolution + Debug> TypeResolution for Vec<T> {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        let mut ret = Ok(());
        for stmt in self {
            let typ = stmt.resolve_type(ctx);
            match typ {
                Ok(_)
                | Err(TranslationError::UnknownReturnType(_))
                | Err(TranslationError::UnknownShape(_)) => (),
                x => ret = x,
            }
        }
        ret
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
            let typ = self.functions.resolve_type(&mut ctx);
            match typ {
                Ok(_)
                | Err(TranslationError::UnknownReturnType(_))
                | Err(TranslationError::UnknownShape(_)) => (),
                Err(x) => return Err(x),
            }
            let new_size = ctx.size();
            if new_size == prev_size {
                break;
            }
            prev_size = new_size;
        }
        Ok(ctx)
    }
}
