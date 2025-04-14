use std::{collections::HashSet, ops::Add};

use viper::AstFactory;

use crate::{
    ir,
    utils::{ExprSubstitution, Shape, ToType, TranslationError, TryToShape, ViperUtils},
};

use super::{
    expression::{Expr, Struct},
    shared::SharedOpType,
    statement::MemOpBytes,
    Arg, BinOp, BinOpType, Decl, Model, Program, SharedPerm, ShiftType, Type, UnOpType,
};

impl Expr {
    pub fn flatten(&self, ctx: &crate::utils::TypeContext) -> Result<Vec<Expr>, TranslationError> {
        match self.to_shape(ctx)? {
            Shape::Simple => Ok(vec![self.clone()]),
            Shape::Nested(shape) => Ok(match self {
                Expr::Struct(struc) => struc.flatten(),
                _ => shape
                    .iter()
                    .enumerate()
                    .flat_map(|(field_idx, _s)| {
                        Expr::Field(ir::Field {
                            field_idx,
                            obj: Box::new(self.clone()),
                        })
                        .flatten(ctx)
                    })
                    .flatten()
                    .collect(),
            }),
        }
    }
}

impl Struct {
    pub fn new(elements: Vec<Expr>) -> Self {
        Self { elements }
    }

    pub fn flatten(&self) -> Vec<Expr> {
        self.elements
            .iter()
            .flat_map(|e| match e {
                Expr::Struct(inner) => inner.flatten(),
                x => vec![x.clone()],
            })
            .collect()
    }
}

impl MemOpBytes {
    pub fn bits(&self) -> u32 {
        match self {
            Self::Byte => 8,
            Self::QuarterWord => 16,
            Self::HalfWord => 32,
            Self::Word => 64,
        }
    }

    pub fn bytes(&self) -> u32 {
        self.bits() / 8
    }
}

impl From<u64> for MemOpBytes {
    fn from(value: u64) -> Self {
        match value {
            8 => Self::Byte,
            16 => Self::QuarterWord,
            32 => Self::HalfWord,
            64 => Self::Word,
            x => panic!("invalid conversion from u64 to MemOpBytes, got {}", x),
        }
    }
}

impl From<Decl> for Arg {
    fn from(value: Decl) -> Self {
        Self {
            name: value.name,
            typ: value.typ,
        }
    }
}

impl Type {
    pub fn len(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Int | Type::Bool => 1,
            Type::Struct(inner) => inner.iter().map(Shape::len).sum(),
            _ => panic!("Unbounded length"),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ToType for BinOpType {
    fn to_type(&self, is_annot: bool) -> super::Type {
        use BinOpType::*;
        match self {
            ViperEqual | ViperNotEqual | Iff | Imp => Type::Bool,
            Gt | Gte | Lt | Lte | SignedGt | SignedGte | SignedLt | SignedLte | BoolAnd
            | BoolOr | PancakeEqual | PancakeNotEqual => {
                if is_annot {
                    Type::Bool
                } else {
                    Type::Int
                }
            }
            Add | Sub | Mul | Div | Modulo | BitOr | BitAnd | BitXor => Type::Int,
        }
    }
}

impl ToType for UnOpType {
    fn to_type(&self, is_annot: bool) -> super::Type {
        match self {
            UnOpType::Minus => Type::Int,
            UnOpType::Neg => {
                if is_annot {
                    Type::Bool
                } else {
                    Type::Int
                }
            }
        }
    }
}

impl Add<i64> for Expr {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        Expr::BinOp(BinOp {
            optype: BinOpType::Add,
            left: Box::new(self),
            right: Box::new(Expr::Const(rhs)),
        })
    }
}

impl UnOpType {
    pub fn eval(&self, value: i64) -> i64 {
        match self {
            Self::Minus => -value,
            Self::Neg => {
                if value == 0 {
                    1
                } else {
                    0
                }
            }
        }
    }
}

impl BinOpType {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::Add | Self::Sub | Self::Div | Self::Mul | Self::Modulo
        )
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(self, Self::BitAnd | Self::BitOr | Self::BitXor)
    }

    pub fn is_bool(&self) -> bool {
        !(self.is_bitwise() || self.is_bitwise())
    }

    pub fn eval(&self, lhs: i64, rhs: i64) -> i64 {
        match self {
            Self::Add => lhs + rhs,
            Self::BitAnd => lhs & rhs,
            Self::BitOr => lhs | rhs,
            Self::BitXor => lhs ^ rhs,
            Self::Div => lhs / rhs,
            Self::Modulo => lhs % rhs,
            Self::Mul => lhs * rhs,
            Self::Sub => lhs - rhs,
            x => {
                let b = match x {
                    Self::BoolAnd => (lhs != 0) && (rhs != 0),
                    Self::BoolOr => (lhs != 0) || (rhs != 0),
                    Self::Gt => lhs > rhs,
                    Self::Gte => lhs >= rhs,
                    Self::Lt => lhs < rhs,
                    Self::Lte => lhs <= rhs,
                    Self::Iff => (lhs != 0) == (rhs != 0),
                    Self::Imp => (lhs == 0) || (rhs != 0),
                    Self::PancakeEqual | Self::ViperEqual => lhs == rhs,
                    Self::PancakeNotEqual | Self::ViperNotEqual => lhs != rhs,
                    _ => unreachable!(),
                };
                if b {
                    1
                } else {
                    0
                }
            }
        }
    }
}

impl ShiftType {
    pub fn eval(&self, lhs: i64, rhs: u64) -> i64 {
        match self {
            ShiftType::Asr => lhs >> rhs,
            ShiftType::Lsl => lhs << rhs,
            ShiftType::Lsr => ((lhs as u64) >> rhs) as i64,
        }
    }
}

impl ExprSubstitution for Expr {
    fn substitute(&mut self, old: &Expr, new: &Expr) -> bool {
        if *self == *old {
            *self = new.clone();
            return true;
        }
        match self {
            Self::AccessPredicate(acc) => acc.field.substitute(old, new),
            Self::AccessSlice(acc) => {
                let a = acc.field.substitute(old, new);
                let b = acc.lower.substitute(old, new);
                let c = acc.upper.substitute(old, new);
                a || b || c
            }
            Self::ArrayAccess(acc) => {
                let a = acc.obj.substitute(old, new);
                let b = acc.idx.substitute(old, new);
                a || b
            }
            Self::BinOp(op) => {
                let a = op.left.substitute(old, new);
                let b = op.right.substitute(old, new);
                a || b
            }
            Self::UnOp(op) => op.right.substitute(old, new),
            Self::Field(field) => field.obj.substitute(old, new),
            Self::Load(load) => load.address.substitute(old, new),
            Self::LoadBits(load) => load.address.substitute(old, new),
            Self::Old(o) => o.expr.substitute(old, new),
            Self::SeqLength(s) => s.expr.substitute(old, new),
            Self::Shift(shift) => shift.value.substitute(old, new),
            Self::Struct(s) => s.elements.substitute(old, new),
            Self::Ternary(tern) => {
                let a = tern.cond.substitute(old, new);
                let b = tern.left.substitute(old, new);
                let c = tern.right.substitute(old, new);
                a || b || c
            }
            Self::MethodCall(call) => call.args.substitute(old, new),
            Self::FunctionCall(call) => call.args.substitute(old, new),
            Self::Quantified(quant) => quant.body.substitute(old, new),
            Self::UnfoldingIn(fold) => {
                let a = fold.pred.substitute(old, new);
                let b = fold.expr.substitute(old, new);
                a || b
            }
            Self::ViperFieldAccess(acc) => acc.obj.substitute(old, new),
            Self::BaseAddr
            | Self::BoolLit(_)
            | Self::Const(_)
            | Self::Var(_)
            | Self::Label(_)
            | Self::BytesInWord => false,
        }
    }
}

impl ExprSubstitution for Vec<Expr> {
    fn substitute(&mut self, old: &ir::Expr, new: &ir::Expr) -> bool {
        let mut acc = false;
        self.iter_mut()
            .for_each(|e| acc = acc || e.substitute(old, new));
        acc
    }
}

impl From<Arg> for Expr {
    fn from(value: Arg) -> Self {
        Expr::Var(value.name)
    }
}

impl SharedPerm {
    pub fn is_read(&self) -> bool {
        matches!(self, Self::ReadOnly | Self::ReadWrite)
    }

    pub fn is_write(&self) -> bool {
        matches!(self, Self::WriteOnly | Self::ReadWrite)
    }

    pub fn is_allowed(&self, op: SharedOpType) -> bool {
        match op {
            SharedOpType::Load => self.is_read(),
            SharedOpType::Store => self.is_write(),
        }
    }
}

impl Program {
    pub fn get_method_names(&self) -> (Vec<String>, Vec<String>) {
        let fnames = self.functions.iter().map(|f| f.fname.to_owned());
        let shared_names = self.shared.iter().flat_map(|s| match s.typ {
            SharedPerm::ReadOnly => vec![format!("load_{}", s.name)],
            SharedPerm::WriteOnly => vec![format!("store_{}", s.name)],
            SharedPerm::ReadWrite => {
                vec![format!("load_{}", s.name), format!("store_{}", s.name)]
            }
        });
        (fnames.collect(), shared_names.collect())
    }

    pub fn exclude_functions(&mut self, exclude_list: &[String]) {
        self.functions
            .iter_mut()
            .filter(|f| exclude_list.contains(&f.fname))
            .for_each(|f| f.trusted = true);
    }

    pub fn trust_except(&mut self, include_list: &[String]) {
        let fnames = self.get_method_names().0;
        let fnames_set = fnames.into_iter().collect::<HashSet<_>>();
        let include_set = include_list.iter().cloned().collect::<HashSet<_>>();
        let exclude_list = fnames_set
            .difference(&include_set)
            .cloned()
            .collect::<Vec<_>>();
        self.exclude_functions(&exclude_list);
    }
}

impl Model {
    pub fn get_default_args<'a>(
        &self,
        ast: AstFactory<'a>,
        heap_var: (viper::LocalVarDecl<'a>, viper::Expr<'a>),
    ) -> (Vec<viper::LocalVarDecl<'a>>, Vec<viper::Expr<'a>>) {
        std::iter::once(heap_var)
            .chain(
                self.fields
                    .iter()
                    .map(|arg| ast.new_var(arg, ast.ref_type())),
            )
            .unzip()
    }
}
