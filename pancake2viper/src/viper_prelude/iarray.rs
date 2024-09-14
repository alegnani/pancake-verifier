use viper::{AstFactory, Domain, DomainFunc, Expr, Field, Method, Predicate, Stmt, Type};

use crate::utils::ViperUtils;

#[derive(Clone, Copy)]
pub struct IArrayHelper<'a> {
    ast: AstFactory<'a>,
    pub domain: Domain<'a>,
    // copy_slice_m: Method<'a>,
    pub len_f: DomainFunc<'a>,
    pub slot_f: DomainFunc<'a>,
}

impl<'a> IArrayHelper<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        let domain_name = "IArray";
        let iarray_type = ast.domain_type(domain_name, &[], &[]);
        let (a_decl, a) = ast.new_var("a", iarray_type);
        let (r_decl, r) = ast.new_var("r", ast.ref_type());
        let (i_decl, i) = ast.new_var("i", ast.int_type());

        let slot_f = ast.domain_func(
            "slot",
            &[a_decl, i_decl],
            ast.ref_type(),
            false,
            domain_name,
        );
        let len_f = ast.domain_func("len", &[a_decl], ast.int_type(), false, domain_name);
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
    pub fn slot_f(&self, array: Expr, idx: Expr) -> Expr<'a> {
        self.ast.domain_func_app(self.slot_f, &[array, idx], &[])
    }

    /// Encodes the following function application to get the length of an IArray
    /// ```viper
    /// len(array)
    /// ```
    pub fn len_f(&self, array: Expr) -> Expr<'a> {
        self.ast.domain_func_app(self.len_f, &[array], &[])
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
    /// predicate slice_acc(src: IArray, l: Int, h: Int) {
    ///     forall j: Int ::l <= j < h <= len(src) ==> acc(slot(src, j).heap_elem)
    /// }
    /// ```
    pub fn slice_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (l_decl, l) = ast.new_var("l", ast.int_type());
        let (h_decl, h) = ast.new_var("h", ast.int_type());
        self.ast.predicate(
            "slice_acc",
            &[src_decl, l_decl, h_decl],
            Some(self.array_acc_expr(src, l, h)),
        )
    }

    /// Encodes the following predicate for full access of an IArray
    /// ```viper
    /// predicate slice_acc(src: IArray, l: Int, h: Int) {
    ///     forall j: Int ::0 <= j < len(src) ==> acc(slot(src, j).heap_elem)
    /// }
    /// ```
    pub fn full_acc_def(&self) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let l = ast.int_lit(0);
        let h = self.len_f(src);
        self.ast.predicate(
            "slice_acc",
            &[src_decl],
            Some(self.array_acc_expr(src, l, h)),
        )
    }

    /// Encodes the following expression for permissions of an IArray (slice)
    /// ```viper
    ///     forall j: Int ::0 <= j < len(src) ==> acc(slot(src, j).heap_elem)
    /// ```
    pub fn array_acc_expr(&self, array: Expr, l: Expr, h: Expr) -> Expr<'a> {
        let ast = self.ast;
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let zero = ast.int_lit(0);
        let upper = self.len_f(array);

        let l0 = ast.le_cmp(zero, l);
        let lj = ast.le_cmp(l, j);
        let jh = ast.lt_cmp(j, h);
        let hu = ast.le_cmp(h, upper);
        let guard = ast.and(ast.and(l0, lj), ast.and(jh, hu));

        let access = ast.acc(ast.field_access(self.slot_f(array, j), self.field()));

        ast.forall(&[j_decl], &[], ast.implies(guard, access))
    }

    /// Encodes the following method for copying a slice of an IArray
    /// ```viper
    /// method copy_slice(src: IArray, l: Int, h: Int) returns  (dst: IArray)
    ///     requires 0 <= l <= h < len(src)
    ///     requires slice_acc(src, l, h)
    ///     ensures len(dst) == h - l
    ///     ensures slice_acc(src, l, h)
    ///     ensures forall i: Int :: l <= i < h ==> old(slot(src, i).heap_elem) == slot(src, i).heap_elem
    ///     ensures full_acc(dst)
    ///     ensures forall i: Int :: 0 <= i < h - l ==> slot(src, l + i).heap_elem == slot(dst, i).heap_elem
    /// ```
    pub fn copy_slice_def(&self) -> Method<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (l_decl, l) = ast.new_var("l", ast.int_type());
        let (h_decl, h) = ast.new_var("h", ast.int_type());
        let (dst_decl, dst) = ast.new_var("dst", self.get_type());
        let (i_decl, i) = ast.new_var("i", ast.int_type());
        let zero = ast.int_lit(0);

        let pres = [
            ast.le_cmp(zero, l),
            ast.le_cmp(l, h),
            ast.le_cmp(h, self.len_f(src)),
            self.array_acc_expr(src, l, h),
        ];

        let src_guard = ast.and(ast.le_cmp(l, i), ast.lt_cmp(i, h));
        let slot_i = ast.field_access(self.slot_f(src, i), self.field());
        let src_impl = ast.eq_cmp(ast.old(slot_i), slot_i);

        let dst_guard = ast.and(ast.le_cmp(zero, i), ast.lt_cmp(i, ast.sub(h, l)));
        let dst_impl = ast.eq_cmp(
            ast.field_access(self.slot_f(src, ast.add(l, i)), self.field()),
            ast.field_access(self.slot_f(dst, i), self.field()),
        );

        let posts = [
            ast.eq_cmp(self.len_f(dst), ast.sub(h, l)),
            self.array_acc_expr(src, l, h),
            ast.forall(&[i_decl], &[], ast.implies(src_guard, src_impl)),
            self.array_acc_expr(dst, zero, self.len_f(dst)),
            ast.forall(&[i_decl], &[], ast.implies(dst_guard, dst_impl)),
        ];

        ast.method(
            "copy_slice",
            &[src_decl, l_decl, h_decl],
            &[dst_decl],
            &pres,
            &posts,
            None,
        )
    }

    /// Encodes the following method application for copying a slice of an IArray
    /// ```viper
    /// dst := copy_slice(src, l, h)
    /// ```
    pub fn copy_slice_m(&self, src: Expr, l: Expr, h: Expr, dst: Expr) -> Stmt<'a> {
        self.ast.method_call("copy_slice", &[src, l, h], &[dst])
    }
}
