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
}

impl ExprTypeResolution for ir::Expr {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        use ir::Expr::*;
        match self {
            Const(_) | LoadBits(_) | Shift(_) | BaseAddr | BytesInWord | FieldAccessChain(_) => {
                Ok(Type::Int)
            }
            BoolLit(_) => Ok(Type::Bool),
            ArrayAccess(acc) => acc.resolve_expr_type(ctx),
            BinOp(op) => op.resolve_expr_type(ctx),
            UnOp(op) => op.resolve_expr_type(ctx),
            AccessPredicate(_) | AccessSlice(_) => Ok(Type::Bool),
            Var(name) => ctx.get_type_no_mangle(name),
            Label(_) => unreachable!(),
            Struct(struc) => Ok(struc.to_shape(ctx)?.to_type()),
            Field(field) => Ok(field.to_shape(ctx)?.to_type()),
            Load(load) => Ok(load.shape.to_type()),
            MethodCall(call) => ctx.get_function_type(&call.fname),
            FunctionCall(call) => ctx.get_function_type(&call.fname),
            UnfoldingIn(fold) => fold.expr.resolve_expr_type(ctx),
            Ternary(tern) => tern.left.resolve_expr_type(ctx),
            Quantified(quant) => {
                quant.decls.resolve_type(ctx)?;
                Ok(Type::Bool)
            }
            Old(old) => old.expr.resolve_expr_type(ctx),
        }
    }
}

impl ExprTypeResolution for ir::ArrayAccess {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        let obj_type = self.obj.resolve_expr_type(ctx)?;
        assert_eq!(self.idx.resolve_expr_type(ctx)?, Type::Int);
        match obj_type {
            Type::Struct(inner) => Ok(match *self.idx {
                Expr::Const(i) => inner[i as usize].to_type(),
                _ => {
                    let mut shapes = inner.iter();
                    if let Some(head) = shapes.next() {
                        assert!(shapes.all(|s| s == head), "Each nested struct, indexed by a non-constant value needs to have the same type");
                    }
                    inner[0].to_type()
                }
            }),
            Type::Array => Ok(Type::Int),
            _ => Err(TranslationError::ShapeError(IRSimpleShapeFieldAccess(
                *self.obj.clone(),
            ))),
        }
    }
}

impl ExprTypeResolution for ir::UnOp {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        self.right.resolve_expr_type(ctx)?;
        Ok(self.optype.to_type())
    }
}

impl ExprTypeResolution for ir::BinOp {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        self.left.resolve_expr_type(ctx)?;
        self.right.resolve_expr_type(ctx)?;
        Ok(self.optype.to_type())
    }
}

impl TypeResolution for ir::Annotation {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.expr.resolve_expr_type(ctx)?;
        Ok(())
    }
}

impl TypeResolution for ir::Stmt {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        match self {
            ir::Stmt::Definition(def) => {
                match def.rhs.to_shape(ctx) {
                    Ok(shape) => ctx.set_type(def.lhs.clone(), shape.to_type()),
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
            ir::Stmt::Assign(ass) => {
                let rhs_type = ass.rhs.resolve_expr_type(ctx)?;
                ctx.set_type(ass.lhs.clone(), rhs_type);
                Ok(())
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

impl<T: ExprTypeResolution> ExprTypeResolution for Vec<T> {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        for expr in self {
            ignore_unknown(expr.resolve_expr_type(ctx).map(|_| ()))?;
        }
        Ok(Type::Wildcard)
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

impl<T: ExprTypeResolution> ExprTypeResolution for Option<T> {
    fn resolve_expr_type(&self, ctx: &mut TypeContext) -> Result<Type, TranslationError> {
        match self {
            Some(t) => t.resolve_expr_type(ctx),
            None => Ok(Type::Void),
        }
    }
}

impl TypeResolution for ir::Arg {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.clone());
        Ok(())
    }
}

impl TypeResolution for ir::FnDec {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.pres.resolve_expr_type(ctx)?;
        self.posts.resolve_expr_type(ctx)?;
        self.body.resolve_type(ctx)?;
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
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        ctx.set_type(self.name.clone(), self.typ.clone());
        Ok(())
    }
}

impl TypeResolution for ir::Predicate {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.body.resolve_expr_type(ctx)?;
        ctx.set_type(self.name.clone(), Type::Bool);
        Ok(())
    }
}

impl TypeResolution for ir::Function {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.body.resolve_expr_type(ctx)?;
        self.pres.resolve_expr_type(ctx)?;
        self.posts.resolve_expr_type(ctx)?;
        ctx.set_type(self.name.clone(), self.typ.clone());
        Ok(())
    }
}

impl TypeResolution for ir::AbstractMethod {
    fn resolve_type(&self, ctx: &mut TypeContext) -> Result<(), TranslationError> {
        self.args.resolve_type(ctx)?;
        self.pres.resolve_expr_type(ctx)?;
        self.posts.resolve_expr_type(ctx)?;
        self.rettyps.resolve_type(ctx)?;
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

impl ir::Program {
    pub fn resolve_types(&self) -> Result<TypeContext, TranslationError> {
        let mut ctx = TypeContext::new();
        let mut prev_size = ctx.size();
        for pred in &self.state {
            if let Expr::FunctionCall(call) = pred {
                ctx.set_type(call.fname.clone(), Type::Bool);
            }
        }
        loop {
            ignore_unknown(self.viper_functions.resolve_type(&mut ctx))?;
            ignore_unknown(self.predicates.resolve_type(&mut ctx))?;
            ignore_unknown(self.methods.resolve_type(&mut ctx))?;
            ignore_unknown(self.functions.resolve_type(&mut ctx))?;
            ignore_unknown(self.state.resolve_expr_type(&mut ctx).map(|_| ()))?;
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
