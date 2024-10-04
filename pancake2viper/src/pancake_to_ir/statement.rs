use crate::{annotation::parse_annot, ir, pancake};

use super::utils::Wrapper;

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

impl From<pancake::Declaration> for ir::Definition {
    fn from(value: pancake::Declaration) -> Self {
        Self {
            lhs: value.lhs,
            rhs: value.rhs.into(),
            scope: Box::new((*value.scope).into()),
        }
    }
}

impl From<pancake::Store> for ir::Store {
    fn from(value: pancake::Store) -> Self {
        Self {
            address: value.address.into(),
            value: value.value.into(),
        }
    }
}

impl From<pancake::MemOpBytes> for ir::MemOpBytes {
    fn from(value: pancake::MemOpBytes) -> Self {
        use pancake::MemOpBytes::*;
        match value {
            Byte => Self::Byte,
            HalfWord => Self::HalfWord,
        }
    }
}

// XXX: do we want to only have one load and store and then a masked read/write?
impl From<pancake::StoreBits> for ir::StoreBits {
    fn from(value: pancake::StoreBits) -> Self {
        Self {
            address: value.address.into(),
            value: value.value.into(),
            size: value.size.into(),
        }
    }
}

impl From<pancake::SharedStore> for ir::SharedStore {
    fn from(value: pancake::SharedStore) -> Self {
        Self {
            address: value.address.into(),
            value: value.value.into(),
        }
    }
}

impl From<pancake::SharedStoreBits> for ir::SharedStoreBits {
    fn from(value: pancake::SharedStoreBits) -> Self {
        Self {
            address: value.address.into(),
            value: value.value.into(),
            size: value.size.into(),
        }
    }
}

impl From<pancake::SharedLoad> for ir::SharedLoad {
    fn from(value: pancake::SharedLoad) -> Self {
        Self {
            address: value.address.into(),
            dst: value.dst.into(),
        }
    }
}

impl From<pancake::SharedLoadBits> for ir::SharedLoadBits {
    fn from(value: pancake::SharedLoadBits) -> Self {
        Self {
            address: value.address.into(),
            size: value.size.into(),
            dst: value.dst.into(),
        }
    }
}

impl From<pancake::Seq> for ir::Seq {
    fn from(value: pancake::Seq) -> Self {
        let stmts: Wrapper<ir::Stmt> = value.stmts.into();
        Self { stmts: stmts.0 }
    }
}

impl From<pancake::If> for ir::If {
    fn from(value: pancake::If) -> Self {
        Self {
            cond: value.cond.into(),
            if_branch: Box::new((*value.if_branch).into()),
            else_branch: Box::new((*value.else_branch).into()),
        }
    }
}

impl From<pancake::While> for ir::While {
    fn from(value: pancake::While) -> Self {
        Self {
            cond: value.cond.into(),
            body: Box::new((*value.body).into()),
        }
    }
}

impl From<pancake::Call> for ir::Call {
    fn from(value: pancake::Call) -> Self {
        let args: Wrapper<ir::Expr> = value.args.into();
        Self {
            call: ir::Expr::MethodCall(ir::MethodCall {
                fname: Box::new(value.fname.into()),
                rettype: crate::shape::Shape::Simple, // FIXME
                args: args.0,
            }),
        }
    }
}

impl From<pancake::TailCall> for ir::Return {
    fn from(value: pancake::TailCall) -> Self {
        let args: Wrapper<ir::Expr> = value.args.into();
        ir::Return {
            value: ir::Expr::MethodCall(ir::MethodCall {
                fname: Box::new(value.fname.into()),
                args: args.0,
                rettype: crate::shape::Shape::Simple, // FIXME
            }),
        }
    }
}

impl From<pancake::ExtCall> for ir::ExtCall {
    fn from(value: pancake::ExtCall) -> Self {
        let args = value.args.map(|a| a.into()).to_vec();
        Self {
            fname: value.fname,
            args,
        }
    }
}

impl From<pancake::Return> for ir::Return {
    fn from(value: pancake::Return) -> Self {
        Self {
            value: value.value.into(),
        }
    }
}

impl From<pancake::Stmt> for ir::Stmt {
    fn from(value: pancake::Stmt) -> Self {
        use pancake::Stmt::*;
        match value {
            Skip => Self::Skip,
            Annotation(a) => Self::Annotation(parse_annot(&a.line)),
            Declaration(d) => Self::Definition(d.into()),
            Assign(a) => Self::Assign(a.into()),
            Store(s) => Self::Store(s.into()),
            StoreBits(s) => Self::StoreBits(s.into()),
            SharedStore(s) => Self::SharedStore(s.into()),
            SharedStoreBits(s) => Self::SharedStoreBits(s.into()),
            SharedLoad(s) => Self::SharedLoad(s.into()),
            SharedLoadBits(s) => Self::SharedLoadBits(s.into()),
            Seq(s) => Self::Seq(s.into()),
            If(i) => Self::If(i.into()),
            While(w) => Self::While(w.into()),
            Break => Self::Break,
            Continue => Self::Continue,
            Call(c) => Self::Call(c.into()),
            TailCall(t) => Self::Return(t.into()),
            ExtCall(e) => Self::ExtCall(e.into()),
            Raise(_) | Tick => panic!("Raise and Tick are not implemented"),
            Return(r) => Self::Return(r.into()),
        }
    }
}
