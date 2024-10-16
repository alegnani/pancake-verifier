use crate::{
    ir,
    utils::{Mangleable, Mangler, TranslationError, TranslationMode, VariableType},
};

impl<T: Mangleable> Mangleable for Vec<T> {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        self.iter_mut().try_for_each(|e| e.mangle(mangler))
    }
}

impl<T: Mangleable> Mangleable for Option<T> {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        match self {
            Some(e) => e.mangle(mangler),
            None => Ok(()),
        }
    }
}

impl Mangleable for ir::Decl {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        self.name = mangler.new_mangled_var(self.name.clone(), VariableType::Variable)?;
        Ok(())
    }
}

impl Mangleable for ir::Expr {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        use ir::Expr::*;
        match self {
            Const(_) | BaseAddr | BytesInWord => (),
            Var(name) => *name = mangler.mangle_var(name)?.to_owned(),
            Label(label) => *label = Mangler::mangle_fn(label),
            Struct(struc) => struc.elements.mangle(mangler)?,
            Field(field) => field.obj.mangle(mangler)?,
            Load(load) => load.address.mangle(mangler)?,
            LoadByte(load) => load.address.mangle(mangler)?,
            UnOp(op) => op.right.mangle(mangler)?,
            BinOp(op) => {
                op.left.mangle(mangler)?;
                op.right.mangle(mangler)?
            }
            Shift(shift) => shift.value.mangle(mangler)?,
            MethodCall(call) => {
                call.fname = Mangler::mangle_fn(&call.fname);
                call.args.mangle(mangler)?
            }
            FunctionCall(call) => {
                call.fname = Mangler::mangle_fn(&call.fname);
                call.args.mangle(mangler)?
            }
            Quantified(quant) => {
                quant.decls.mangle(mangler)?;
                quant.body.mangle(mangler)?
            }
            ArrayAccess(access) => {
                access.obj.mangle(mangler)?;
                access.idx.mangle(mangler)?
            }
            AccessPredicate(acc) => acc.field.mangle(mangler)?,
            FieldAccessChain(access) => access.obj.mangle(mangler)?,
            UnfoldingIn(fold) => {
                fold.expr.mangle(mangler)?;
                fold.pred.mangle(mangler)?
            }
            Ternary(tern) => {
                tern.cond.mangle(mangler)?;
                tern.left.mangle(mangler)?;
                tern.right.mangle(mangler)?
            }
        }
        Ok(())
    }
}

impl Mangleable for ir::Annotation {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        mangler.mangle_mode(self.typ.into());
        self.expr.mangle(mangler)?;
        mangler.mangle_mode(TranslationMode::Normal);
        mangler.clear_annot_var();
        Ok(())
    }
}

impl Mangleable for ir::Stmt {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        use ir::Stmt::*;
        match self {
            Skip | Break | Continue => (),
            Annotation(annot) => annot.mangle(mangler)?,
            Definition(def) => {
                def.rhs.mangle(mangler)?;
                def.lhs = mangler.new_mangled_var(def.lhs.clone(), VariableType::Variable)?;
                def.scope.mangle(mangler)?
            }
            Assign(ass) => {
                ass.lhs = mangler.mangle_var(&ass.lhs)?.to_owned();
                ass.rhs.mangle(mangler)?
            }
            Store(store) => {
                store.address.mangle(mangler)?;
                store.value.mangle(mangler)?
            }
            StoreBits(store) => {
                store.address.mangle(mangler)?;
                store.value.mangle(mangler)?
            }
            SharedStore(store) => {
                store.address.mangle(mangler)?;
                store.value.mangle(mangler)?
            }
            SharedStoreBits(store) => {
                store.address.mangle(mangler)?;
                store.value.mangle(mangler)?
            }
            SharedLoad(load) => {
                load.address.mangle(mangler)?;
                load.dst.mangle(mangler)?
            }
            SharedLoadBits(load) => {
                load.address.mangle(mangler)?;
                load.dst.mangle(mangler)?
            }
            Seq(seq) => seq.stmts.mangle(mangler)?,
            If(i) => {
                i.cond.mangle(mangler)?;
                i.if_branch.mangle(mangler)?;
                i.else_branch.mangle(mangler)?;
            }
            While(w) => {
                w.cond.mangle(mangler)?;
                w.body.mangle(mangler)?
            }
            Call(call) => call.call.mangle(mangler)?,
            ExtCall(call) => {
                call.args.mangle(mangler)?;
                call.fname = Mangler::mangle_fn(&call.fname)
            }
            Return(r) => r.value.mangle(mangler)?,
        };
        Ok(())
    }
}

impl Mangleable for ir::Arg {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        self.name = mangler.new_mangled_var(self.name.clone(), VariableType::Argument)?;
        Ok(())
    }
}

impl Mangleable for ir::FnDec {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        mangler.switch_ctx(self.fname.clone());
        self.fname = Mangler::mangle_fn(&self.fname);
        self.args.mangle(mangler)?;
        self.body.mangle(mangler)
    }
}

impl Mangleable for ir::Predicate {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        mangler.switch_ctx(self.name.clone());
        self.name = Mangler::mangle_fn(&self.name);
        self.args.mangle(mangler)?;
        self.body.mangle(mangler)
    }
}

impl Mangleable for ir::Function {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        mangler.switch_ctx(self.name.clone());
        self.name = Mangler::mangle_fn(&self.name);
        self.args.mangle(mangler)?;
        self.preposts.mangle(mangler)?;
        self.body.mangle(mangler)
    }
}

impl Mangleable for ir::AbstractMethod {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        mangler.switch_ctx(self.name.clone());
        self.name = Mangler::mangle_fn(&self.name);
        self.args.mangle(mangler)?;
        self.preposts.mangle(mangler)
    }
}

impl Mangleable for ir::Program {
    fn mangle(&mut self, mangler: &mut Mangler) -> Result<(), TranslationError> {
        self.viper_functions.mangle(mangler)?;
        self.predicates.mangle(mangler)?;
        self.methods.mangle(mangler)?;
        self.functions
            .iter_mut()
            .try_for_each(|e| e.mangle(&mut mangler.clone()))
    }
}
