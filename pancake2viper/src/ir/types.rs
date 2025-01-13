use std::rc::Rc;

use crate::{
    ir::{self, Expr},
    utils::{
        ExprTypeResolution, Shape, ShapeError::IRSimpleShapeFieldAccess, ToType, TranslationError,
        TryToShape, TypeContext, TypeResolution,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Int,
    Bool,
    Struct(Vec<Shape>),
    Array,
    Wildcard,
    Ref,
    Set(Box<Self>),
    Seq(Box<Self>),
    Map(Box<Self>, Box<Self>),
}

impl ExprTypeResolution for ir::Expr {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        use ir::Expr::*;
        match self {
            Const(_) | LoadBits(_) | Shift(_) | BaseAddr | BytesInWord => Ok(Type::Int),
            BoolLit(_) if is_annot => Ok(Type::Bool),
            BoolLit(_) => Ok(Type::Int),
            ArrayAccess(acc) => acc.resolve_expr_type(is_annot, ctx),
            BinOp(op) => op.resolve_expr_type(is_annot, ctx),
            UnOp(op) => op.resolve_expr_type(is_annot, ctx),
            AccessPredicate(_) | AccessSlice(_) => Ok(Type::Bool),
            Var(name) => ctx.get_type_no_mangle(name),
            Label(_) => unreachable!(),
            Struct(struc) => Ok(struc.to_shape(ctx)?.to_type(is_annot)),
            Field(field) => Ok(field.to_shape(ctx)?.to_type(is_annot)),
            Load(load) => Ok(load.shape.to_type(is_annot)),
            MethodCall(call) => ctx.get_function_type(&call.fname),
            FunctionCall(call) => ctx.get_function_type(&call.fname),
            UnfoldingIn(fold) => fold.expr.resolve_expr_type(is_annot, ctx),
            Ternary(tern) => tern.left.resolve_expr_type(is_annot, ctx),
            Quantified(quant) => {
                quant.decls.resolve_type(is_annot, ctx)?;
                Ok(Type::Bool)
            }
            Old(old) => old.expr.resolve_expr_type(is_annot, ctx),
            ViperFieldAccess(acc) => ctx.get_field_type(&acc.field),
        }
    }
}

impl ExprTypeResolution for ir::ArrayAccess {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        let obj_type = self.obj.resolve_expr_type(is_annot, ctx)?;
        assert_eq!(self.idx.resolve_expr_type(is_annot, ctx)?, Type::Int);
        match obj_type {
            Type::Struct(inner) => Ok(match *self.idx {
                Expr::Const(i) => inner[i as usize].to_type(is_annot),
                _ => {
                    let mut shapes = inner.iter();
                    if let Some(head) = shapes.next() {
                        assert!(shapes.all(|s| s == head), "Each nested struct, indexed by a non-constant value needs to have the same type");
                    }
                    inner[0].to_type(is_annot)
                }
            }),
            Type::Array => Ok(Type::Int),
            Type::Seq(i) => Ok(*i),
            _ => Err(TranslationError::ShapeError(IRSimpleShapeFieldAccess(
                *self.obj.clone(),
            ))),
        }
    }
}

impl ExprTypeResolution for ir::UnOp {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        self.right.resolve_expr_type(is_annot, ctx)?;
        Ok(self.optype.to_type(is_annot))
    }
}

impl ExprTypeResolution for ir::BinOp {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        self.left.resolve_expr_type(is_annot, ctx)?;
        self.right.resolve_expr_type(is_annot, ctx)?;
        Ok(self.optype.to_type(is_annot))
    }
}

impl TypeResolution for ir::Annotation {
    fn resolve_type(&self, is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.expr.resolve_expr_type(is_annot, ctx)?;
        Ok(())
    }
}

impl TypeResolution for ir::Stmt {
    fn resolve_type(&self, is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            ir::Stmt::Definition(def) => {
                match def.rhs.to_shape(ctx) {
                    Ok(shape) => ctx.set_type(def.lhs.clone(), shape.to_type(is_annot)),
                    Err(TranslationError::UnknownReturnType(_))
                    | Err(TranslationError::UnknownShape(_)) => (), // TODO: do something
                    Err(e) => return Err(e),
                }
                def.scope.resolve_type(is_annot, ctx)
            }
            ir::Stmt::If(i) => {
                let if_type = i.if_branch.resolve_type(is_annot, ctx);
                ignore_unknown(if_type)?;
                i.else_branch.resolve_type(is_annot, ctx)
            }
            ir::Stmt::Assign(ass) => {
                let rhs_type = ass.rhs.resolve_expr_type(is_annot, ctx)?;
                ctx.set_type(ass.lhs.clone(), rhs_type);
                Ok(())
            }
            ir::Stmt::Annotation(annot) => annot.resolve_type(true, ctx),
            ir::Stmt::While(w) => w.body.resolve_type(is_annot, ctx),
            ir::Stmt::Seq(seq) => seq.stmts.resolve_type(is_annot, ctx),
            _ => Ok(()),
        }
    }
}

impl<T: TypeResolution> TypeResolution for Vec<T> {
    fn resolve_type(&self, is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        let mut ret = Ok(());
        for stmt in self {
            let typ = stmt.resolve_type(is_annot, ctx);
            if let Err(e) = ignore_unknown(typ) {
                ret = Err(e);
            }
        }
        ret
    }
}

impl<T: ExprTypeResolution> ExprTypeResolution for Vec<T> {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        let mut ret = Ok(Type::Wildcard);
        for expr in self {
            if let Err(e) = ignore_unknown(expr.resolve_expr_type(is_annot, ctx).map(|_| ())) {
                ret = Err(e);
            }
        }
        ret
    }
}

impl<T: TypeResolution> TypeResolution for Option<T> {
    fn resolve_type(&self, is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            Some(t) => t.resolve_type(is_annot, ctx),
            None => Ok(()),
        }
    }
}

impl<T: ExprTypeResolution> ExprTypeResolution for Option<T> {
    fn resolve_expr_type(
        &self,
        is_annot: bool,
        ctx: &mut TypeContext,
    ) -> Result<Type, TranslationError> {
        match self {
            Some(t) => t.resolve_expr_type(is_annot, ctx),
            None => Ok(Type::Void),
        }
    }
}

impl TypeResolution for ir::Arg {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.clone());
        Ok(())
    }
}

impl TypeResolution for ir::FnDec {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(false, ctx)?;
        self.pres.resolve_expr_type(true, ctx)?;
        self.posts.resolve_expr_type(true, ctx)?;
        self.body.resolve_type(false, ctx)?;
        let ret_type = ctx.get_type_no_mangle(&self.retvar);
        match ret_type {
            Ok(typ) => {
                ctx.set_type(self.fname.clone(), typ);
                Ok(())
            }
            Err(TranslationError::UnknownShape(_)) => {
                Err(TranslationError::UnknownReturnType(self.fname.clone()))
            }
            Err(e) => Err(e),
        }
    }
}

impl TypeResolution for ir::Decl {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.clone());
        Ok(())
    }
}

impl TypeResolution for ir::Predicate {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), Type::Bool);
        self.args.resolve_type(true, ctx)?;
        self.body.resolve_expr_type(true, ctx)?;
        Ok(())
    }
}

impl TypeResolution for ir::Function {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.clone());
        self.args.resolve_type(true, ctx)?;
        self.body.resolve_expr_type(true, ctx)?;
        self.pres.resolve_expr_type(true, ctx)?;
        self.posts.resolve_expr_type(true, ctx)?;
        Ok(())
    }
}

impl TypeResolution for ir::AbstractMethod {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(true, ctx)?;
        self.pres.resolve_expr_type(true, ctx)?;
        self.posts.resolve_expr_type(true, ctx)?;
        self.rettyps.resolve_type(true, ctx)?;
        assert!(self.rettyps.len() <= 1); // TODO: add support for multiple returns
        let shape = if self.rettyps.is_empty() {
            Type::Void
        } else {
            ctx.get_type_no_mangle(&self.rettyps[0].name).unwrap()
        };
        ctx.set_type(self.name.clone(), shape);
        Ok(())
    }
}

impl TypeResolution for ir::Model {
    fn resolve_type(&self, _is_annot: bool, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.predicates.resolve_expr_type(true, ctx).map(|_| ())
    }
}

impl ir::Program {
    pub fn resolve_types(&self) -> Result<TypeContext, TranslationError> {
        let mut ctx = TypeContext::new(Rc::new(self.extern_fields.clone()));
        let mut prev_size = ctx.size();

        for ffi in &self.extern_methods {
            ctx.set_type(ffi.clone(), Type::Int);
        }
        for field in &self.model.fields {
            ctx.set_type(field.to_string(), Type::Ref);
        }
        for pred in &self.model.predicates {
            if let Expr::FunctionCall(call) = pred {
                ctx.set_type(call.fname.clone(), Type::Bool);
            }
        }
        for pred in &self.extern_predicates {
            ctx.set_type(format!("f_{}", pred), Type::Bool);
        }
        loop {
            ignore_unknown(self.viper_functions.resolve_type(true, &mut ctx))?;
            ignore_unknown(self.predicates.resolve_type(true, &mut ctx))?;
            ignore_unknown(self.methods.resolve_type(true, &mut ctx))?;
            ignore_unknown(self.functions.resolve_type(false, &mut ctx))?;
            ignore_unknown(self.model.resolve_type(true, &mut ctx))?;
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
