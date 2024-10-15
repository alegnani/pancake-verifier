use crate::{
    annotation::parse_annot,
    ir, pancake,
    utils::{Mangler, TranslationError, TryToIR, VariableType},
};

impl From<pancake::Annotation> for ir::Annotation {
    fn from(value: pancake::Annotation) -> Self {
        parse_annot(&value.line)
    }
}

impl TryToIR for pancake::Assign {
    type Output = ir::Assign;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            lhs: mangler.mangle_var(&self.lhs)?.to_owned(),
            rhs: self.rhs.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::Declaration {
    type Output = ir::Definition;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            lhs: mangler.new_mangled_var(self.lhs, VariableType::Variable)?,
            rhs: self.rhs.to_ir(mangler)?,
            scope: Box::new(self.scope.to_ir(mangler)?),
        })
    }
}

impl TryToIR for pancake::MemOpBytes {
    type Output = ir::MemOpBytes;

    fn to_ir(self, _ctx: &mut Mangler) -> Result<Self::Output, TranslationError> {
        use pancake::MemOpBytes::*;
        Ok(match self {
            Byte => Self::Output::Byte,
            HalfWord => Self::Output::HalfWord,
        })
    }
}

impl TryToIR for pancake::Store {
    type Output = ir::Store;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            value: self.value.to_ir(mangler)?,
        })
    }
}

// XXX: do we want to only have one load and store and then a masked read/write?
impl TryToIR for pancake::StoreBits {
    type Output = ir::StoreBits;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            value: self.value.to_ir(mangler)?,
            size: self.size.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::SharedStore {
    type Output = ir::SharedStore;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            value: self.value.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::SharedStoreBits {
    type Output = ir::SharedStoreBits;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            value: self.value.to_ir(mangler)?,
            size: self.size.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::SharedLoad {
    type Output = ir::SharedLoad;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            dst: self.dst.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::SharedLoadBits {
    type Output = ir::SharedLoadBits;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir(mangler)?,
            dst: self.dst.to_ir(mangler)?,
            size: self.size.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::Seq {
    type Output = ir::Seq;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        let stmts = self.stmts.to_ir(mangler)?;
        Ok(Self::Output { stmts })
    }
}

impl TryToIR for pancake::If {
    type Output = ir::If;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            cond: self.cond.to_ir(mangler)?,
            if_branch: Box::new(self.if_branch.to_ir(mangler)?),
            else_branch: Box::new(self.else_branch.to_ir(mangler)?),
        })
    }
}

impl TryToIR for pancake::While {
    type Output = ir::While;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            cond: self.cond.to_ir(mangler)?,
            body: Box::new(self.body.to_ir(mangler)?),
        })
    }
}

impl TryToIR for pancake::Return {
    type Output = ir::Return;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            value: self.value.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::Call {
    type Output = ir::Call;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir(mangler)?;
        Ok(Self::Output {
            call: ir::Expr::MethodCall(ir::MethodCall {
                fname: self.fname.get_label()?,
                args,
            }),
        })
    }
}

impl TryToIR for pancake::TailCall {
    type Output = ir::Return;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir(mangler)?;
        Ok(Self::Output {
            value: ir::Expr::MethodCall(ir::MethodCall {
                fname: self.fname.get_label()?,
                args,
            }),
        })
    }
}

impl TryToIR for pancake::ExtCall {
    type Output = ir::ExtCall;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir(mangler)?;
        Ok(Self::Output {
            fname: self.fname,
            args,
        })
    }
}

impl TryToIR for pancake::Stmt {
    type Output = ir::Stmt;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, crate::utils::TranslationError> {
        use pancake::Stmt::*;
        Ok(match self {
            Skip => Self::Output::Skip,
            Annotation(annot) => Self::Output::Annotation(parse_annot(&annot.line)), // FIXME
            Declaration(decl) => Self::Output::Definition(decl.to_ir(mangler)?),
            Assign(ass) => Self::Output::Assign(ass.to_ir(mangler)?),
            Store(store) => Self::Output::Store(store.to_ir(mangler)?),
            StoreBits(store) => Self::Output::StoreBits(store.to_ir(mangler)?),
            SharedStore(store) => Self::Output::SharedStore(store.to_ir(mangler)?),
            SharedStoreBits(store) => Self::Output::SharedStoreBits(store.to_ir(mangler)?),
            SharedLoad(load) => Self::Output::SharedLoad(load.to_ir(mangler)?),
            SharedLoadBits(load) => Self::Output::SharedLoadBits(load.to_ir(mangler)?),
            Seq(seq) => Self::Output::Seq(seq.to_ir(mangler)?),
            If(i) => Self::Output::If(i.to_ir(mangler)?),
            While(w) => Self::Output::While(w.to_ir(mangler)?),
            Break => Self::Output::Break,
            Continue => Self::Output::Continue,
            Return(r) => Self::Output::Return(r.to_ir(mangler)?),
            Call(call) => Self::Output::Call(call.to_ir(mangler)?),
            TailCall(call) => Self::Output::Return(call.to_ir(mangler)?),
            ExtCall(call) => Self::Output::ExtCall(call.to_ir(mangler)?),
            Raise(_) | Tick => panic!("Raise and Tick are not implemented in Pancake"),
        })
    }
}
