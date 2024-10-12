use viper::{AstFactory, Domain, DomainFunc, Expr, Field, Method, Predicate, Stmt, Type};

use crate::utils::ViperUtils;

#[derive(Clone, Copy)]
pub struct IArrayHelper<'a> {
    ast: AstFactory<'a>,
    pub domain: Domain<'a>,
    pub len_f: DomainFunc<'a>,
    pub slot_f: DomainFunc<'a>,
}

impl<'a> IArrayHelper<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        let domain_name = "IArray";
        let iarray_type = ast.domain_type(domain_name, &[], &[]);
        let (a_decl, a) = ast.new_var("a", iarray_type);
        let (r_decl, _r) = ast.new_var("r", ast.ref_type());
        let (i_decl, i) = ast.new_var("i", ast.int_type());

        let slot_f = ast.domain_func(
            "slot",
            &[a_decl, i_decl],
            ast.ref_type(),
            false,
            domain_name,
        );
        let len_f = ast.domain_func("alen", &[a_decl], ast.int_type(), false, domain_name);
        let first_f = ast.domain_func("first", &[r_decl], iarray_type, false, domain_name);
        let second_f = ast.domain_func("second", &[r_decl], ast.int_type(), false, domain_name);
        let functions = [slot_f, len_f, first_f, second_f];

        let slot_a_i_app = ast.domain_func_app(slot_f, &[a, i], &[]);

        let first_app = ast.domain_func_app(first_f, &[slot_a_i_app], &[]);
        let second_app = ast.domain_func_app(second_f, &[slot_a_i_app], &[]);
        let len_app = ast.domain_func_app(len_f, &[a], &[]);

        let all_diff_ax = ast.named_domain_axiom(
            "all_diff",
            ast.forall(
                &[a_decl, i_decl],
                &[ast.trigger(&[slot_a_i_app])],
                ast.and(ast.eq_cmp(first_app, a), ast.eq_cmp(second_app, i)),
            ),
            domain_name,
        );

        let len_nonneg_ax = ast.named_domain_axiom(
            "len_nonneg",
            ast.forall(
                &[a_decl],
                &[ast.trigger(&[len_app])],
                ast.ge_cmp(len_app, ast.int_lit(0)),
            ),
            domain_name,
        );

        let axioms = [all_diff_ax, len_nonneg_ax];

        let domain = ast.domain(domain_name, &functions, &axioms, &[]);
        Self {
            ast,
            domain,
            len_f,
            slot_f,
        }
    }

    /// Encodes the following function application to access an element of an IArray
    /// ```viper
    /// slot(array, idx)
    /// ```
    fn slot_f(&self, array: Expr, idx: Expr) -> Expr<'a> {
        self.ast.domain_func_app(self.slot_f, &[array, idx], &[])
    }

    /// Encodes the following function application to get the length of an IArray
    /// ```viper
    /// alen(array)
    /// ```
    pub fn len_f(&self, array: Expr) -> Expr<'a> {
        self.ast.domain_func_app(self.len_f, &[array], &[])
    }

    /// Encodes array accesses of an IArray
    /// ```viper
    /// slot(array, idx).heap_elem
    /// ```
    pub fn access(&self, array: Expr, idx: Expr) -> Expr<'a> {
        self.ast.field_access(self.slot_f(array, idx), self.field())
    }

    /// The type of an IArray
    pub fn get_type(&self) -> Type<'a> {
        self.ast.domain_type("IArray", &[], &[])
    }

    /// The field of an IArray
    pub fn field(&self) -> Field<'a> {
        self.ast.field("heap_elem", self.ast.int_type())
    }

    /// Encodes the following predicate for slice access of an IArray
    /// ```viper
    /// predicate slice_acc(src: IArray, idx: Int, length: Int) {
    ///     forall j: Int :: 0 <= idx <= j < idx + length <= alen(src) ==> acc(slot(src, j).heap_elem)
    /// }
    /// ```
    pub fn slice_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (idx_decl, idx) = ast.new_var("idx", ast.int_type());
        let (length_decl, length) = ast.new_var("length", ast.int_type());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        self.ast.predicate(
            "slice_acc",
            &[src_decl, idx_decl, length_decl, perm_decl],
            Some(self.array_acc_expr(src, idx, length, perm)),
        )
    }

    /// Encodes the following predicate for full access of an IArray
    /// ```viper
    /// predicate slice_acc(src: IArray, l: Int, h: Int) {
    ///     forall j: Int ::0 <= j < alen(src) ==> acc(slot(src, j).heap_elem)
    /// }
    /// ```
    pub fn full_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        let l = ast.int_lit(0);
        let h = self.len_f(src);
        self.ast.predicate(
            "slice_acc",
            &[src_decl, perm_decl],
            Some(self.array_acc_expr(src, l, h, perm)),
        )
    }

    /// Encodes the following expression for permissions of an IArray (slice)
    /// ```viper
    ///     forall j: Int :: 0 <= idx <= j < idx + length <= alen(src) ==> acc(slot(src, j).heap_elem)
    /// ```
    pub fn array_acc_expr(&self, array: Expr, idx: Expr, length: Expr, perm: Expr) -> Expr<'a> {
        let ast = self.ast;
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let upper = self.len_f(array);
        let zero = ast.int_lit(0);
        let limit = ast.add(idx, length);

        let i0 = ast.le_cmp(zero, idx);
        let ij = ast.le_cmp(idx, j);
        let jl = ast.lt_cmp(j, limit);
        let lu = ast.le_cmp(limit, upper);
        let guard = ast.and(ast.and(i0, ij), ast.and(jl, lu));

        let access = ast.field_access_predicate(self.access(array, j), perm);

        ast.forall(&[j_decl], &[], ast.implies(guard, access))
    }

    /// Encodes the following methods for copying and creating a slice of an IArray
    /// ```viper
    /// method copy_slice(src: IArray, src_idx: Int, dst: IArray, dst_idx: Int, length: Int)
    ///     requires 0 <= src_idx <= alen(src)
    ///     requires 0 <= dst_idx <= alen(dst)
    ///     requires src_idx + length <= alen(src)
    ///     requires dst_idx + length <= alen(dst)
    ///     requires slice_access(src, src_idx, length)
    ///     requires slice_access(dst, dst_idx, length)
    ///
    ///     ensures slice_access(src, src_idx, length)
    ///     ensures slice_access(dst, dst_idx, length)
    ///     ensures forall i : Int :: 0 <= i < length ==>
    ///         old(slot(src, src_idx + i).heap_elem) == slot(src, src_idx + i).heap_elem
    ///     ensures forall i : Int :: 0 <= i < length ==>
    ///         slot(src, src_idx + i).heap_elem == slot(dst, dst_idx + i).heap_elem
    ///
    ///
    /// method create_slice(src: IArray, src_idx: Int, dst: IArray, dst_idx: Int, length: Int)
    ///     requires 0 <= src_idx <= alen(src)
    ///     requires 0 <= dst_idx <= alen(dst)
    ///     requires src_idx + length <= alen(src)
    ///     requires dst_idx + length <= alen(dst)
    ///     requires slice_access(src, src_idx, length)
    ///     requires slice_access(dst, dst_idx, length)
    ///
    ///     ensures slice_access(src, src_idx, length)
    ///     ensures slice_access(dst, dst_idx, length)
    ///     ensures forall i : Int :: 0 <= i < length ==>
    ///         old(slot(src, src_idx + i).heap_elem) == slot(src, src_idx + i).heap_elem
    ///     ensures forall i : Int :: 0 <= i < length ==>
    ///         slot(src, src_idx + i).heap_elem == slot(dst, dst_idx + i).heap_elem
    /// ```
    pub fn slice_defs(&self) -> Vec<Method<'a>> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (src_idx_decl, src_idx) = ast.new_var("src_idx", ast.int_type());
        let (dst_decl, dst) = ast.new_var("dst", self.get_type());
        let (dst_idx_decl, dst_idx) = ast.new_var("dst_idx", ast.int_type());
        let (length_decl, length) = ast.new_var("length", ast.int_type());
        let (i_decl, i) = ast.new_var("i", ast.int_type());
        let (j_decl, j) = ast.new_var("j", ast.int_type());

        let zero = ast.int_lit(0);
        let len_src = self.len_f(src);
        let len_dst = self.len_f(dst);
        let read_perm = ast.fractional_perm(ast.int_lit(1), ast.int_lit(2));

        let pres = [
            // requires 0 <= src_idx <= alen(src)
            ast.le_cmp(zero, src_idx),
            ast.le_cmp(src_idx, len_src),
            // requires src_idx + length <= alen(src)
            ast.le_cmp(ast.add(src_idx, length), len_src),
            // requires slice_access(src, src_idx, length, wildcard)
            self.array_acc_expr(src, src_idx, length, read_perm),
            // requires 0 <= dst_idx <= alen(dst)
            ast.le_cmp(zero, dst_idx),
            ast.le_cmp(dst_idx, len_dst),
            // requires dst_idx + length <= alen(dst)
            ast.le_cmp(ast.add(dst_idx, length), len_dst),
            // requires slice_access(dst, dst_idx, length, write)
            self.array_acc_expr(dst, dst_idx, length, ast.full_perm()),
        ];

        let guard_src = ast.and(
            ast.le_cmp(src_idx, i),
            ast.lt_cmp(i, ast.add(src_idx, length)),
        );
        let guard_src_dst = ast.and(
            guard_src,
            ast.eq_cmp(ast.sub(i, j), ast.sub(src_idx, dst_idx)),
        );
        let guard_length = ast.and(ast.le_cmp(zero, i), ast.lt_cmp(i, length));

        let src_impl = ast.eq_cmp(ast.old(self.access(src, i)), self.access(src, i));
        let dst_impl = ast.eq_cmp(self.access(src, i), self.access(dst, j));

        let posts = [
            // ensures slice_access(src, src_idx, length, wildcard)
            self.array_acc_expr(src, src_idx, length, read_perm),
            // ensures slice_access(dst, dst_idx, length, write)
            self.array_acc_expr(dst, dst_idx, length, ast.full_perm()),
            // ensures forall i: Int :: src_idx <= i < src_idx + length ==> old(slot(src, i).heap_elem) == slot(src, i).heap_elem
            ast.forall(&[i_decl], &[], ast.implies(guard_src, src_impl)),
            // ensures forall i: Int, j :Int ::
            //     src_idx <= i < src_idx + length
            //  && i - j == src_idx - dst_idx
            //  ==> slot(src, i).heap_elem == slot(dst, j).heap_elem
            ast.forall(&[i_decl, j_decl], &[], ast.implies(guard_src_dst, dst_impl)),
        ];

        let copy_slice = ast.method(
            "copy_slice",
            &[src_decl, src_idx_decl, dst_decl, dst_idx_decl, length_decl],
            &[],
            &pres,
            &posts,
            None,
        );

        let create_impl = ast.eq_cmp(self.access(src, ast.add(src_idx, i)), self.access(dst, i));

        let create_posts = [
            // length == alen(dst)
            ast.eq_cmp(length, len_dst),
            // ensures slice_access(src, src_idx, length)
            posts[0],
            // ensures full_access(dst, write)
            self.array_acc_expr(dst, zero, length, ast.full_perm()),
            // ensures forall i: Int :: src_idx <= i < src_idx + length ==> old(slot(src, i).heap_elem) == slot(src, i).heap_elem
            posts[2],
            // ensures forall i: Int :: 0 <= i < length ==> slot(src, src_idx + i).heap_elem == slot(dst, i).heap_elem
            ast.forall(&[i_decl], &[], ast.implies(guard_length, create_impl)),
        ];

        let body = ast.seqn(
            &[
                // inhale alen(dst) == length
                ast.inhale(ast.eq_cmp(length, len_dst), ast.no_position()),
                // inhale full_access(dst, write)
                ast.inhale(
                    self.array_acc_expr(dst, zero, length, ast.full_perm()),
                    ast.no_position(),
                ),
                // copy_slice(src, idx, dst, 0, length)
                self.copy_slice_m(src, src_idx, dst, zero, length),
            ],
            &[],
        );

        let create_slice = ast.method(
            "create_slice",
            &[src_decl, src_idx_decl, length_decl],
            &[dst_decl],
            &pres[0..4],
            &create_posts,
            Some(body),
        );
        vec![copy_slice, create_slice]
    }

    /// Encodes the following method application for copying a slice of an IArray
    /// ```viper
    /// copy_slice(src, src_idx, dst, dst_idx, length)
    /// ```
    pub fn copy_slice_m(
        &self,
        src: Expr,
        src_idx: Expr,
        dst: Expr,
        dst_idx: Expr,
        length: Expr,
    ) -> Stmt<'a> {
        self.ast
            .method_call("copy_slice", &[src, src_idx, dst, dst_idx, length], &[])
    }

    /// Encodes the following method application for creating a slice of an IArray
    /// ```viper
    /// dst := create_slice(src, idx, length)
    /// ```
    pub fn create_slice_m(&self, src: Expr, idx: Expr, length: Expr, dst: Expr) -> Stmt<'a> {
        self.ast
            .method_call("create_slice", &[src, idx, length], &[dst])
    }
}
