use crate::{
    ir, pancake,
    utils::{TranslationError, TryToIR, TryToIRGeneric},
};

impl TryToIR for pancake::Struct {
    type Output = ir::Struct;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        Ok(Self::Output {
            elements: self.elements.to_ir()?,
        })
    }
}

impl TryToIR for pancake::Field {
    type Output = ir::Field;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        Ok(Self::Output {
            obj: Box::new(self.obj.to_ir()?),
            field_idx: self.field_idx,
        })
    }
}

impl TryToIR for pancake::Load {
    type Output = ir::Load;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        Ok(Self::Output {
            shape: self.shape,
            address: Box::new(self.address.to_ir()?),
            assert: true,
        })
    }
}

impl TryToIR for pancake::LoadByte {
    type Output = ir::LoadByte;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        Ok(Self::Output {
            address: Box::new(self.address.to_ir()?),
        })
    }
}

impl TryToIRGeneric<ir::UnOpType> for pancake::OpType {
    fn to_ir(self) -> Result<ir::UnOpType, TranslationError> {
        match self {
            pancake::OpType::Sub => Ok(ir::UnOpType::Minus),
            _ => panic!("Can't convert Pancake operator '{:?}' to UnOpType", self),
        }
    }
}

impl TryToIRGeneric<ir::BinOpType> for pancake::OpType {
    fn to_ir(self) -> Result<ir::BinOpType, TranslationError> {
        use ir::BinOpType;
        use pancake::OpType::*;
        Ok(match self {
            Add => BinOpType::Add,
            Sub => BinOpType::Sub,
            Mul => BinOpType::Mul,
            NotEqual => BinOpType::PancakeNotEqual,
            Equal => BinOpType::PancakeEqual,
            Less => BinOpType::Lt,
            NotLess => BinOpType::Gte,
            And => BinOpType::BitAnd,
            Or => BinOpType::BitOr,
            Xor => BinOpType::BitXor,
        })
    }
}

impl TryToIRGeneric<ir::UnOp> for pancake::Op {
    fn to_ir(mut self) -> Result<ir::UnOp, TranslationError> {
        assert!(self.operands.len() == 1);
        let optype = TryToIRGeneric::to_ir(self.optype)?;
        let right = Box::new(self.operands.remove(0).to_ir()?);
        Ok(ir::UnOp { optype, right })
    }
}

impl TryToIRGeneric<ir::BinOp> for pancake::Op {
    fn to_ir(self) -> Result<ir::BinOp, TranslationError> {
        assert!(self.operands.len() >= 2);
        let optype = TryToIRGeneric::to_ir(self.optype)?;
        let mut iter = self.operands.into_iter();
        let acc = ir::BinOp {
            optype,
            left: Box::new(iter.next().unwrap().to_ir()?),
            right: Box::new(iter.next().unwrap().to_ir()?),
        };
        iter.try_fold(acc, |acc, op| {
            Ok(ir::BinOp {
                optype,
                left: Box::new(ir::Expr::BinOp(acc)),
                right: Box::new(op.to_ir()?),
            })
        })
    }
}

impl TryToIR for pancake::ShiftType {
    type Output = ir::ShiftType;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        use pancake::ShiftType::*;
        Ok(match self {
            Lsl => Self::Output::Lsl,
            Asr => Self::Output::Asr,
            Lsr => Self::Output::Lsr,
        })
    }
}

impl TryToIR for pancake::Shift {
    type Output = ir::Shift;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            shifttype: self.shifttype.to_ir()?,
            value: Box::new(self.value.to_ir()?),
            amount: self.amount,
        })
    }
}

impl TryToIR for pancake::ExprCall {
    type Output = ir::MethodCall;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        Ok(Self::Output {
            fname: self.fname.get_label()?,
            args,
        })
    }
}

impl TryToIR for pancake::Expr {
    type Output = ir::Expr;

    fn to_ir(self) -> Result<Self::Output, crate::utils::TranslationError> {
        use pancake::Expr::*;
        Ok(match self {
            Const(c) => Self::Output::Const(c),
            Var(varname) => Self::Output::Var(varname),
            Label(label) => Self::Output::Label(label),
            Struct(struc) => Self::Output::Struct(struc.to_ir()?),
            Field(field) => Self::Output::Field(field.to_ir()?),
            Load(load) => Self::Output::Load(load.to_ir()?),
            LoadByte(load) => Self::Output::LoadByte(load.to_ir()?),
            Op(o) if o.operands.len() == 1 => Self::Output::UnOp(o.to_ir()?),
            Op(o) => Self::Output::BinOp(o.to_ir()?),
            Shift(shift) => Self::Output::Shift(shift.to_ir()?),
            BaseAddr => Self::Output::BaseAddr,
            BytesInWord => Self::Output::BytesInWord,
            Call(call) => Self::Output::MethodCall(call.to_ir()?),
        })
    }
}
