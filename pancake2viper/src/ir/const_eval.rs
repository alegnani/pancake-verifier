use crate::{
    ir::{self, ShiftType},
    utils::{ConstEval, ConstEvalExpr, EncodeOptions},
};

use super::{
    AbstractMethod, BinOp, BinOpType, Expr, FnDec, Function, Predicate, Program, Shared, Shift,
    Stmt, UnOp, UnOpType,
};

impl ConstEvalExpr for Expr {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        use Expr::*;
        match self {
            x @ (Const(_) | BoolLit(_) | Label(_) | Var(_)) => x,
            // x @ Var(s) => match const_map.entry(s) {
            //     Entry::Occupied(v) => Const(*v.get()),
            //     Entry::Vacant(_) => x,
            // },
            Struct(s) => Struct(ir::Struct {
                elements: const_eval_vec(s.elements, options),
            }),
            Field(f) => Field(ir::Field {
                field_idx: f.field_idx,
                obj: Box::new(f.obj.const_eval(options)),
            }),
            Load(l) => Load(ir::Load {
                shape: l.shape,
                address: Box::new(l.address.const_eval(options)),
                assert: l.assert,
            }),
            LoadBits(l) => LoadBits(ir::LoadBits {
                address: Box::new(l.address.const_eval(options)),
                size: l.size,
            }),
            BinOp(b) => b.const_eval(options),
            UnOp(u) => u.const_eval(options),
            Shift(s) => s.const_eval(options),
            BaseAddr => Const(0),
            BytesInWord => Const(options.word_size as i64 / 8),
            MethodCall(m) => MethodCall(ir::MethodCall {
                fname: m.fname,
                args: const_eval_vec(m.args, options),
            }),
            FunctionCall(f) => FunctionCall(ir::FunctionCall {
                fname: f.fname,
                args: const_eval_vec(f.args, options),
            }),
            Quantified(q) => Quantified(ir::Quantified {
                quantifier: q.quantifier,
                decls: q.decls,
                triggers: q.triggers,
                body: Box::new(q.body.const_eval(options)),
            }),
            ArrayAccess(a) => ArrayAccess(ir::ArrayAccess {
                obj: Box::new(a.obj.const_eval(options)),
                idx: Box::new(a.idx.const_eval(options)),
            }),
            AccessPredicate(a) => AccessPredicate(ir::AccessPredicate {
                field: Box::new(a.field.const_eval(options)),
                perm: a.perm,
            }),
            FieldAccessChain(f) => FieldAccessChain(ir::FieldAccessChain {
                obj: Box::new(f.obj.const_eval(options)),
                idxs: f.idxs,
            }),
            UnfoldingIn(u) => UnfoldingIn(ir::UnfoldingIn {
                pred: Box::new(u.pred.const_eval(options)),
                expr: Box::new(u.expr.const_eval(options)),
            }),
            Ternary(t) => Ternary(ir::Ternary {
                cond: Box::new(t.cond.const_eval(options)),
                left: Box::new(t.left.const_eval(options)),
                right: Box::new(t.right.const_eval(options)),
            }),
            AccessSlice(a) => AccessSlice(ir::AccessSlice {
                field: Box::new(a.field.const_eval(options)),
                lower: Box::new(a.lower.const_eval(options)),
                upper: Box::new(a.upper.const_eval(options)),
                typ: a.typ,
                perm: a.perm,
            }),
            Old(o) => Old(ir::Old {
                expr: Box::new(o.expr.const_eval(options)),
            }),
            ViperFieldAccess(f) => ViperFieldAccess(ir::ViperFieldAccess {
                obj: Box::new(f.obj.const_eval(options)),
                field: f.field,
            }),
        }
    }
}

impl ConstEvalExpr for BinOp {
    fn const_eval(self, options: &EncodeOptions) -> Expr {
        let left = self.left.const_eval(options);
        let right = self.right.const_eval(options);
        // XXX: only do const evaluation of arithmetic s.t. we don't break annotations
        match (left, right) {
            (Expr::Const(l), Expr::Const(r))
                if self.optype.is_arithmetic() || self.optype.is_bitwise() =>
            {
                Expr::Const(self.optype.eval(l, r))
            }
            (l, Expr::Const(r))
                if self.optype == BinOpType::BitAnd && (r + 1).count_ones() == 1 =>
            {
                Expr::BinOp(BinOp {
                    optype: BinOpType::Modulo,
                    left: Box::new(l),
                    right: Box::new(Expr::Const(r + 1)),
                })
            }
            (l, r) => Expr::BinOp(BinOp {
                optype: self.optype,
                left: Box::new(l),
                right: Box::new(r),
            }),
        }
    }
}

impl ConstEvalExpr for UnOp {
    fn const_eval(self, options: &EncodeOptions) -> Expr {
        match *self.right {
            // XXX: only do const evaluation of arithmetic s.t. we don't break annotations
            Expr::Const(i) if matches!(self.optype, UnOpType::Minus) => {
                Expr::Const(self.optype.eval(i))
            }
            _ => Expr::UnOp(UnOp {
                optype: self.optype,
                right: Box::new(self.right.const_eval(options)),
            }),
        }
    }
}

// TODO: check for errors in conversion
// TODO: 32-bit vs 64-bit mode
impl ConstEvalExpr for Shift {
    fn const_eval(self, options: &EncodeOptions) -> Expr {
        let value = self.value.const_eval(options);
        match value {
            Expr::Const(v) => {
                let const_value = match self.shifttype {
                    ShiftType::Asr => v >> self.amount,
                    ShiftType::Lsl => v << self.amount,
                    ShiftType::Lsr => ((v as u64) >> self.amount) as i64,
                };
                Expr::Const(const_value)
            }
            _ => Expr::Shift(Shift {
                shifttype: self.shifttype,
                value: Box::new(value),
                amount: self.amount,
            }),
        }
    }
}

fn const_eval_vec(vec: Vec<Expr>, options: &EncodeOptions) -> Vec<Expr> {
    vec.into_iter().map(|e| e.const_eval(options)).collect()
}

impl ConstEval for Stmt {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        use Stmt::*;
        match self {
            x @ (Skip | Break | Continue | Return) => x,
            Annotation(annot) => Annotation(ir::Annotation {
                typ: annot.typ,
                expr: annot.expr.const_eval(options),
            }),
            Definition(def) => Definition(ir::Definition {
                lhs: def.lhs,
                rhs: def.rhs.const_eval(options),
                scope: Box::new(def.scope.const_eval(options)),
            }),
            Assign(ass) => Assign(ir::Assign {
                lhs: ass.lhs,
                rhs: ass.rhs.const_eval(options),
            }),
            Store(st) => Store(ir::Store {
                address: st.address.const_eval(options),
                value: st.value.const_eval(options),
            }),
            StoreBits(st) => StoreBits(ir::StoreBits {
                address: st.address.const_eval(options),
                value: st.value.const_eval(options),
                size: st.size,
            }),
            SharedStore(st) => SharedStore(ir::SharedStore {
                address: st.address.const_eval(options),
                value: st.value.const_eval(options),
            }),
            SharedStoreBits(st) => SharedStoreBits(ir::SharedStoreBits {
                address: st.address.const_eval(options),
                value: st.value.const_eval(options),
                size: st.size,
            }),
            SharedLoad(ld) => SharedLoad(ir::SharedLoad {
                address: ld.address.const_eval(options),
                dst: ld.dst.const_eval(options),
            }),
            SharedLoadBits(ld) => SharedLoadBits(ir::SharedLoadBits {
                address: ld.address.const_eval(options),
                dst: ld.dst.const_eval(options),
                size: ld.size,
            }),
            Seq(s) => Seq(ir::Seq {
                stmts: s.stmts.const_eval(options),
            }),
            If(i) => If(ir::If {
                cond: i.cond.const_eval(options),
                if_branch: Box::new(i.if_branch.const_eval(options)),
                else_branch: Box::new(i.else_branch.const_eval(options)),
            }),
            While(w) => While(ir::While {
                cond: w.cond.const_eval(options),
                body: Box::new(w.body.const_eval(options)),
            }),
            Call(c) => Call(ir::Call {
                call: c.call.const_eval(options),
            }),
            ExtCall(c) => ExtCall(ir::ExtCall {
                fname: c.fname,
                args: const_eval_vec(c.args, options),
            }),
        }
    }
}

impl<T: ConstEval> ConstEval for Vec<T> {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        self.into_iter().map(|e| e.const_eval(options)).collect()
    }
}

impl ConstEval for Vec<Expr> {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        self.into_iter().map(|e| e.const_eval(options)).collect()
    }
}

impl ConstEval for FnDec {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            pres: self.pres.const_eval(options),
            posts: self.posts.const_eval(options),
            body: self.body.const_eval(options),
            ..self
        }
    }
}

impl ConstEval for Function {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            pres: self.pres.const_eval(options),
            posts: self.posts.const_eval(options),
            body: self.body.map(|b| b.const_eval(options)),
            ..self
        }
    }
}

impl ConstEval for Predicate {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            body: self.body.map(|b| b.const_eval(options)),
            ..self
        }
    }
}

impl ConstEval for AbstractMethod {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            pres: self.pres.const_eval(options),
            posts: self.posts.const_eval(options),
            ..self
        }
    }
}

impl ConstEval for Shared {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            lower: self.lower.const_eval(options),
            upper: self.upper.const_eval(options),
            stride: self.stride.const_eval(options),
            ..self
        }
    }
}

impl ConstEval for Program {
    fn const_eval(self, options: &EncodeOptions) -> Self {
        Self {
            functions: self.functions.const_eval(options),
            methods: self.methods.const_eval(options),
            predicates: self.predicates.const_eval(options),
            viper_functions: self.viper_functions.const_eval(options),
            shared: self.shared.const_eval(options),
            state: self.state.const_eval(options),
            extern_predicates: self.extern_predicates,
            extern_fields: self.extern_fields,
        }
    }
}
