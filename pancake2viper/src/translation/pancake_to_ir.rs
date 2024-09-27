use crate::{annotation::parse_annot, ir, pancake};

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

impl From<pancake::Struct> for ir::Struct {
    fn from(value: pancake::Struct) -> Self {
        let elements: Wrapper<ir::Expr> = value.elements.into();
        Self {
            elements: elements.0,
        }
    }
}

impl From<pancake::Field> for ir::Field {
    fn from(value: pancake::Field) -> Self {
        Self {
            obj: Box::new((*value.obj).into()),
            field_idx: value.field_idx,
        }
    }
}

impl From<pancake::Load> for ir::Load {
    fn from(value: pancake::Load) -> Self {
        Self {
            shape: value.shape,
            address: Box::new((*value.address).into()),
            assert: true,
        }
    }
}

impl From<pancake::LoadByte> for ir::LoadByte {
    fn from(value: pancake::LoadByte) -> Self {
        Self {
            address: Box::new((*value.address).into()),
        }
    }
}

impl From<pancake::OpType> for ir::UnOpType {
    fn from(value: pancake::OpType) -> Self {
        match value {
            pancake::OpType::Sub => Self::Minus,
            _ => panic!("Can't convert Pancake operator '{:?}' to UnOpType", value),
        }
    }
}

impl From<pancake::Op> for ir::UnOp {
    fn from(mut value: pancake::Op) -> Self {
        assert!(value.operands.len() == 1);
        let optype = value.optype.into();
        let right = Box::new(value.operands.remove(0).into());
        Self { optype, right }
    }
}

impl From<pancake::OpType> for ir::BinOpType {
    fn from(value: pancake::OpType) -> Self {
        use pancake::OpType::*;
        match value {
            Add => Self::Add,
            Sub => Self::Sub,
            Mul => Self::Mul,
            NotEqual => Self::PancakeNotEqual,
            Equal => Self::PancakeEqual,
            Less => Self::Lt,
            NotLess => Self::Gte,
            And => Self::BitAnd,
            Or => Self::BitOr,
            Xor => Self::BitXor,
        }
    }
}

impl From<pancake::Op> for ir::BinOp {
    fn from(value: pancake::Op) -> Self {
        assert!(value.operands.len() >= 2);
        let optype = value.optype.into();
        let mut iter = value.operands.into_iter();
        let acc = Self {
            optype,
            left: Box::new(iter.next().unwrap().into()),
            right: Box::new(iter.next().unwrap().into()),
        };
        iter.fold(acc, |acc, op| Self {
            optype,
            left: Box::new(ir::Expr::BinOp(acc)),
            right: Box::new(op.into()),
        })
    }
}

impl From<pancake::ShiftType> for ir::ShiftType {
    fn from(value: pancake::ShiftType) -> Self {
        use pancake::ShiftType::*;
        match value {
            Lsl => Self::Lsl,
            Asr => Self::Asr,
            Lsr => Self::Lsr,
        }
    }
}

impl From<pancake::Shift> for ir::Shift {
    fn from(value: pancake::Shift) -> Self {
        Self {
            shifttype: value.shifttype.into(),
            value: Box::new((*value.value).into()),
            amount: value.amount,
        }
    }
}

impl From<pancake::ExprCall> for ir::MethodCall {
    fn from(value: pancake::ExprCall) -> Self {
        let args: Wrapper<ir::Expr> = value.args.into();
        Self {
            rettype: value.rettype,
            fname: Box::new((*value.fname).into()),
            args: args.0,
        }
    }
}

impl From<pancake::Expr> for ir::Expr {
    fn from(value: pancake::Expr) -> Self {
        use pancake::Expr::*;
        match value {
            Const(i) => Self::Const(i),
            Var(s) => Self::Var(s),
            Label(s) => Self::Label(s),
            Struct(s) => Self::Struct(s.into()),
            Field(f) => Self::Field(f.into()),
            Load(l) => Self::Load(l.into()),
            LoadByte(l) => Self::LoadByte(l.into()),
            Op(o) if o.operands.len() == 1 => Self::UnOp(o.into()),
            Op(o) => Self::BinOp(o.into()),
            Shift(s) => Self::Shift(s.into()),
            BaseAddr => Self::BaseAddr,
            BytesInWord => Self::BytesInWord,
            Call(c) => Self::MethodCall(c.into()),
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
            rettype: value.rettype,
            fname: value.fname.into(),
            args: args.0,
        }
    }
}

// XXX: do we want this in our IR? -> call + return
impl From<pancake::TailCall> for ir::TailCall {
    fn from(value: pancake::TailCall) -> Self {
        let args: Wrapper<ir::Expr> = value.args.into();
        Self {
            fname: value.fname.into(),
            args: args.0,
        }
    }
}

impl From<pancake::ExtCall> for ir::ExtCall {
    fn from(value: pancake::ExtCall) -> Self {
        let args = value.args.map(|a| a.into());
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
            TailCall(t) => Self::TailCall(t.into()),
            ExtCall(e) => Self::ExtCall(e.into()),
            Raise(_) | Tick => panic!("Raise and Tick are not implemented"),
            Return(r) => Self::Return(r.into()),
        }
    }
}

impl From<pancake::Arg> for ir::Arg {
    fn from(value: pancake::Arg) -> Self {
        Self {
            name: value.name,
            shape: value.shape,
        }
    }
}

impl From<pancake::FnDec> for ir::FnDec {
    fn from(value: pancake::FnDec) -> Self {
        let args: Wrapper<ir::Arg> = value.args.into();
        Self {
            fname: value.fname,
            args: args.0,
            body: value.body.into(),
        }
    }
}

impl From<pancake::Program> for ir::Program {
    fn from(value: pancake::Program) -> Self {
        let functions: Wrapper<ir::FnDec> = value.functions.into();
        Self {
            functions: functions.0,
        }
    }
}

struct Wrapper<T>(Vec<T>);

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
