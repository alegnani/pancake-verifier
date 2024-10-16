use crate::{
    annotation::parse_annot,
    ir, pancake,
    utils::{TranslationError, TryToIR},
};

impl From<pancake::Annotation> for ir::Annotation {
    fn from(value: pancake::Annotation) -> Self {
        parse_annot(&value.line)
    }
}

impl TryToIR for pancake::Assign {
    type Output = ir::Assign;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            lhs: self.lhs,
            rhs: self.rhs.to_ir()?,
        })
    }
}

impl TryToIR for pancake::Declaration {
    type Output = ir::Definition;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            lhs: self.lhs,
            rhs: self.rhs.to_ir()?,
            scope: Box::new(self.scope.to_ir()?),
        })
    }
}

impl TryToIR for pancake::MemOpBytes {
    type Output = ir::MemOpBytes;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        use pancake::MemOpBytes::*;
        Ok(match self {
            Byte => Self::Output::Byte,
            HalfWord => Self::Output::HalfWord,
        })
    }
}

impl TryToIR for pancake::Store {
    type Output = ir::Store;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            value: self.value.to_ir()?,
        })
    }
}

// XXX: do we want to only have one load and store and then a masked read/write?
impl TryToIR for pancake::StoreBits {
    type Output = ir::StoreBits;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            value: self.value.to_ir()?,
            size: self.size.to_ir()?,
        })
    }
}

impl TryToIR for pancake::SharedStore {
    type Output = ir::SharedStore;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            value: self.value.to_ir()?,
        })
    }
}

impl TryToIR for pancake::SharedStoreBits {
    type Output = ir::SharedStoreBits;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            value: self.value.to_ir()?,
            size: self.size.to_ir()?,
        })
    }
}

impl TryToIR for pancake::SharedLoad {
    type Output = ir::SharedLoad;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            dst: self.dst.to_ir()?,
        })
    }
}

impl TryToIR for pancake::SharedLoadBits {
    type Output = ir::SharedLoadBits;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            address: self.address.to_ir()?,
            dst: self.dst.to_ir()?,
            size: self.size.to_ir()?,
        })
    }
}

impl TryToIR for pancake::Seq {
    type Output = ir::Seq;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let stmts = self.stmts.to_ir()?;
        Ok(Self::Output { stmts })
    }
}

impl TryToIR for pancake::If {
    type Output = ir::If;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            cond: self.cond.to_ir()?,
            if_branch: Box::new(self.if_branch.to_ir()?),
            else_branch: Box::new(self.else_branch.to_ir()?),
        })
    }
}

impl TryToIR for pancake::While {
    type Output = ir::While;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            cond: self.cond.to_ir()?,
            body: Box::new(self.body.to_ir()?),
        })
    }
}

impl TryToIR for pancake::Return {
    type Output = ir::Seq;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            stmts: vec![
                ir::Stmt::Assign(ir::Assign {
                    lhs: "retval".into(),
                    rhs: self.value.to_ir()?,
                }),
                ir::Stmt::Return,
            ],
        })
    }
}

impl TryToIR for pancake::Call {
    type Output = ir::Call;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        Ok(Self::Output {
            call: ir::Expr::MethodCall(ir::MethodCall {
                fname: self.fname.get_label()?,
                args,
            }),
        })
    }
}

impl TryToIR for pancake::TailCall {
    type Output = ir::Seq;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        Ok(Self::Output {
            stmts: vec![
                ir::Stmt::Assign(ir::Assign {
                    lhs: "retval".into(),
                    rhs: ir::Expr::MethodCall(ir::MethodCall {
                        fname: self.fname.get_label()?,
                        args,
                    }),
                }),
                ir::Stmt::Return,
            ],
        })
    }
}

impl TryToIR for pancake::ExtCall {
    type Output = ir::ExtCall;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        Ok(Self::Output {
            fname: format!("ffi{}", self.fname),
            args,
        })
    }
}

impl TryToIR for pancake::Stmt {
    type Output = ir::Stmt;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        use pancake::Stmt::*;
        Ok(match self {
            Skip => Self::Output::Skip,
            Annotation(annot) => Self::Output::Annotation(parse_annot(&annot.line)), // FIXME
            Declaration(decl) => Self::Output::Definition(decl.to_ir()?),
            Assign(ass) => Self::Output::Assign(ass.to_ir()?),
            Store(store) => Self::Output::Store(store.to_ir()?),
            StoreBits(store) => Self::Output::StoreBits(store.to_ir()?),
            SharedStore(store) => Self::Output::SharedStore(store.to_ir()?),
            SharedStoreBits(store) => Self::Output::SharedStoreBits(store.to_ir()?),
            SharedLoad(load) => Self::Output::SharedLoad(load.to_ir()?),
            SharedLoadBits(load) => Self::Output::SharedLoadBits(load.to_ir()?),
            Seq(seq) => Self::Output::Seq(seq.to_ir()?),
            If(i) => Self::Output::If(i.to_ir()?),
            While(w) => Self::Output::While(w.to_ir()?),
            Break => Self::Output::Break,
            Continue => Self::Output::Continue,
            Return(r) => Self::Output::Seq(r.to_ir()?),
            Call(call) => Self::Output::Call(call.to_ir()?),
            TailCall(call) => Self::Output::Seq(call.to_ir()?),
            ExtCall(call) => Self::Output::ExtCall(call.to_ir()?),
            Raise(_) | Tick => panic!("Raise and Tick are not implemented in Pancake"),
        })
    }
}
