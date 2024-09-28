use super::utils::Wrapper;
use crate::{ir, pancake};

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
