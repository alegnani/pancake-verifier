use std::fmt::{Display, Formatter, Result};

use crate::utils::Shape;

use super::{
    AnnotationType, BinOpType, Decl, Expr, Permission, Quantifier, ShiftType, SliceType, Stmt,
    Type, UnOpType,
};

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Skip => write!(f, "skip;"),
            Self::Break => write!(f, "break;"),
            Self::Continue => write!(f, "continue;"),
            Self::Return => write!(f, "return;"),
            Self::Definition(def) => write!(f, "var {} = {};", def.lhs, def.rhs),
            Self::Assign(ass) => write!(f, "{} = {};", ass.lhs, ass.rhs),
            Self::If(i) => write!(f, "if ({}) ...", i.cond),
            Self::While(w) => write!(f, "while ({}) ...", w.cond),
            Self::Seq(_) => write!(f, "Sequence"),
            Self::Annotation(annot) => write!(f, "/*@ {} {} @*/", annot.typ, annot.expr),
            Self::Store(store) => write!(f, "st {}, {};", store.address, store.value),
            Self::StoreBits(store) => write!(
                f,
                "st{} {}, {};",
                store.size.bits(),
                store.address,
                store.value
            ),
            Self::SharedStore(store) => write!(f, "@st {}, {};", store.address, store.value),
            Self::SharedStoreBits(store) => write!(
                f,
                "@st{} {}, {};",
                store.size.bits(),
                store.address,
                store.value
            ),
            Self::SharedLoad(load) => write!(f, "@ldw {}, {};", load.dst, load.address),
            Self::SharedLoadBits(load) => {
                write!(f, "@ld{} {}, {};", load.size.bits(), load.dst, load.address)
            }
            Self::ExtCall(call) => write!(f, "@{}({})", call.fname, exprs_to_string(&call.args)),
            Self::Call(call) => write!(f, "{}", call.call),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Const(c) => write!(f, "{}", c),
            Self::Var(v) => write!(f, "{}", v),
            Self::Label(l) => write!(f, "{}", l),
            Self::BaseAddr => write!(f, "@base"),
            Self::BytesInWord => write!(f, "@biw"),
            Self::UnOp(op) => write!(f, "{}{}", op.optype, op.right),
            Self::BinOp(op) => write!(f, "(){} {} {})", op.left, op.optype, op.right),
            Self::Shift(shift) => {
                write!(f, "(){} {} {})", shift.value, shift.shifttype, shift.amount)
            }
            Self::Load(load) => write!(f, "(lds {} {})", load.shape, load.address),
            Self::LoadBits(load) => write!(f, "(ld{} {})", load.size.bits(), load.address),
            Self::Struct(s) => write!(f, "<{}>", exprs_to_string(&s.elements)),
            Self::Field(field) => write!(f, "{}.{}", field.obj, field.field_idx),
            Self::Old(old) => write!(f, "old({})", old.expr),
            Self::MethodCall(call) => write!(f, "{}({})", call.fname, exprs_to_string(&call.args)),
            Self::FunctionCall(call) => {
                write!(f, "{}({})", call.fname, exprs_to_string(&call.args))
            }
            Self::Quantified(quant) => write!(
                f,
                "(){} {} :: {})",
                quant.quantifier,
                decls_to_string(&quant.decls),
                quant.body,
            ),
            Self::ArrayAccess(acc) => write!(f, "{}[{}]", acc.obj, acc.idx),
            Self::AccessPredicate(acc) => write!(f, "acc({}, {})", acc.field, acc.perm),
            Self::AccessSlice(acc) => write!(
                f,
                "acc({}[{}{}{}], {})",
                acc.field, acc.lower, acc.typ, acc.upper, acc.perm
            ),
            Self::FieldAccessChain(acc) => {
                write! {f, "{}.{}", acc.obj, acc.idxs.iter().map(usize::to_string).collect::<Vec<_>>().join(".")}
            }
            Self::UnfoldingIn(fold) => write!(f, "(unfolding {} in {})", fold.pred, fold.expr),
            Self::Ternary(t) => write!(f, "(({}) ? {} : {})", t.cond, t.left, t.right),
        }
    }
}

impl Display for Permission {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Read => write!(f, "read"),
            Self::Write => write!(f, "write"),
            Self::Wildcard => write!(f, "wildcard"),
            Self::Fractional(e, d) => write!(f, "{}/{}", e, d),
        }
    }
}

impl Display for ShiftType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Lsl => write!(f, "<<"),
            Self::Asr => write!(f, ">>"),
            Self::Lsr => write!(f, ">>>"),
        }
    }
}

impl Display for UnOpType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Neg => write!(f, "!"),
            Self::Minus => write!(f, "-"),
        }
    }
}

impl Display for BinOpType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Gt => ">",
                Self::Lt => "<",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Imp => "==>",
                Self::Iff => "<==>",
                Self::Gte => ">=",
                Self::Lte => "<=",
                Self::BitOr => "|",
                Self::BitAnd => "&",
                Self::BitXor => "^",
                Self::Modulo => "%",
                Self::BoolOr => "||",
                Self::BoolAnd => "&&",
                Self::ViperEqual => "===",
                Self::ViperNotEqual => "!==",
                Self::PancakeEqual => "==",
                Self::PancakeNotEqual => "!=",
            }
        )
    }
}

impl Display for SliceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Inclusive => write!(f, "..="),
            Self::Exclusive => write!(f, ".."),
        }
    }
}

impl Display for AnnotationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Inhale => "inhale",
                Self::Exhale => "exhale",
                Self::Assertion => "assert",
                Self::Assumption => "assume",
                Self::Invariant => "invariant",
                Self::Refutation => "refute",
                Self::Precondition => "requires",
                Self::Postcondition => "ensures",
                Self::Fold => "fold",
                Self::Unfold => "unfold",
            }
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Struct(s) => write!(f, "{}", Shape::Nested(s.to_vec())),
            Self::Wildcard => write!(f, "*"),
            Self::Void => write!(f, "Void"),
            Self::Array => write!(f, "IArray"),
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.typ)
    }
}

impl Display for Quantifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Forall => write!(f, "forall"),
            Self::Exists => write!(f, "exists"),
        }
    }
}

fn exprs_to_string(exprs: &[Expr]) -> String {
    exprs
        .iter()
        .map(Expr::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

fn decls_to_string(decls: &[Decl]) -> String {
    decls
        .iter()
        .map(Decl::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}
