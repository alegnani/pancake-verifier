use viper::{AstFactory, Expr, Field, Predicate, Type};

use crate::utils::ViperUtils;

#[derive(Clone, Copy)]
pub struct HeapSeq<'a> {
    ast: AstFactory<'a>,
}

impl<'a> HeapSeq<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self { ast }
    }
    ///
    /// The field of a HeapSeq
    pub fn field(&self) -> Field<'a> {
        self.ast.field("heap_elem", self.ast.int_type())
    }

    /// The type of a HeapSeq
    pub fn typ(&self) -> Type<'a> {
        self.ast.seq_type(self.ast.ref_type())
    }

    /// Encodes array accesses for a HeapSeq
    pub fn access(&self, array: Expr, idx: Expr) -> Expr<'a> {
        self.ast
            .field_access(self.ast.seq_index(array, idx), self.field())
    }

    /// Encodes the following predicate for slice access of a HeapSeq
    /// ```viper
    /// predicate slice_acc(src: Seq[Ref], idx: Int, length: Int) {
    ///     forall j: Int :: 0 <= idx <= j < idx + length <= |src| ==> acc(src[j].heap_elem)
    /// }
    /// ```
    pub fn slice_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.typ());
        let (idx_decl, idx) = ast.new_var("idx", ast.int_type());
        let (length_decl, length) = ast.new_var("length", ast.int_type());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        ast.predicate(
            "slice_acc",
            &[src_decl, idx_decl, length_decl, perm_decl],
            Some(self.seq_acc_expr(src, idx, length, perm)),
        )
    }

    /// Encodes the following predicate for full access of a HeapSeq
    /// ```viper
    /// predicate slice_acc(src: Seq[Ref], l: Int, h: Int) {
    ///     forall j: Int ::0 <= j < |src| ==> acc(src[j].heap_elem)
    /// }
    /// ```
    pub fn full_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.typ());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        let l = ast.zero();
        let h = ast.seq_length(src);
        ast.predicate(
            "slice_acc",
            &[src_decl, perm_decl],
            Some(self.seq_acc_expr(src, l, h, perm)),
        )
    }

    /// Encodes the following expression for permissions of a HeapSeq
    /// ```viper
    ///     forall j: Int :: 0 <= idx <= j < idx + length <= |array| ==> acc(array[j].heap_elem, perm)
    /// ```
    pub fn seq_acc_expr(&self, array: Expr, idx: Expr, length: Expr, perm: Expr) -> Expr<'a> {
        let ast = self.ast;
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let upper = ast.seq_length(array);
        let zero = ast.zero();
        let limit = ast.add(idx, length);

        let i0 = ast.le_cmp(zero, idx);
        let ij = ast.le_cmp(idx, j);
        let jl = ast.lt_cmp(j, limit);
        let lu = ast.le_cmp(limit, upper);
        let guard = ast.and(ast.and(i0, ij), ast.and(jl, lu));

        let access = ast.field_access_predicate(ast.seq_index(array, j), perm);

        ast.forall(&[j_decl], &[], ast.implies(guard, access))
    }

    /// Encodes the following expression about the injectivity of a HeapSeq
    /// ```viper
    ///     forall j: Int :: 0 <= i < j < |heap| ==> heap[i] != heap[j]
    /// ```
    pub fn seq_inj(&self, heap: Expr) -> Expr<'a> {
        let ast = self.ast;
        let (i_decl, i) = ast.new_var("i", ast.int_type());
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let upper = ast.seq_length(heap);
        let zero = ast.zero();

        let i0 = ast.le_cmp(zero, i);
        let ij = ast.lt_cmp(i, j);
        let jl = ast.lt_cmp(j, upper);
        let guard = ast.and(ast.and(i0, ij), jl);
        let ineq = ast.ne_cmp(ast.seq_index(heap, i), ast.seq_index(heap, j));

        ast.forall(&[i_decl, j_decl], &[], ast.implies(guard, ineq))
    }
}
