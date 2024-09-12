use viper::{AstFactory, Domain, DomainFunc, Field, Method};

pub fn create_viper_prelude(ast: AstFactory) -> (Vec<Domain>, Vec<Field>, Vec<Method>) {
    let domains = vec![create_array_domain(ast), create_bv_domain(ast)];
    // TODO: split fields into the functions
    let fields = vec![ast.field("heap_elem", ast.int_type())];
    let methods = vec![create_array_method(ast)];
    (domains, fields, methods)
}

fn create_bv_domain(ast: AstFactory) -> Domain {
    let bv64 = ast.backend_bv64_type();
    let from_int = ast.domain_func_with_interpretation(
        "bv64_from_int",
        &[ast.local_var_decl("i", ast.int_type())],
        bv64,
        false,
        "BitVectorDomain64",
        Some("(_ int2bv 64)".into()),
    );
    let to_int = ast.domain_func_with_interpretation(
        "bv64_to_int",
        &[ast.local_var_decl("i", bv64)],
        ast.int_type(),
        false,
        "BitVectorDomain64",
        Some("(_ bv2int 64)".into()),
    );

    ast.domain_with_interpretation(
        "BitVectorDomain64",
        &[
            create_bv_function(ast, "xor"),
            create_bv_function(ast, "and"),
            create_bv_function(ast, "or"),
            create_bv_function(ast, "shl"),
            create_bv_function(ast, "lshr"),
            create_bv_function(ast, "ashr"),
            from_int,
            to_int,
        ],
        &[],
        &[],
        Some(&[
            ("SMTLIB".into(), "(_ BitVec 64)".into()),
            ("Boogie".into(), "bv64".into()),
        ]),
    )
}

fn create_bv_function<'a>(ast: AstFactory<'a>, name: &str) -> DomainFunc<'a> {
    let bv64 = ast.backend_bv64_type();
    let a = ast.local_var_decl("a", bv64);
    let b = ast.local_var_decl("b", bv64);
    ast.domain_func_with_interpretation(
        &format!("bv64_{}", name),
        &[a, b],
        bv64,
        false,
        "BitVectorDomain64",
        Some(format!("bv{}", name)),
    )
}

fn create_array_domain(ast: AstFactory) -> Domain {
    let iarray_type = ast.domain_type("IArray", &[], &[]);
    let a = ast.local_var_decl("a", iarray_type);
    let r = ast.local_var_decl("r", ast.ref_type());
    let i = ast.local_var_decl("i", ast.int_type());

    let slot_f = ast.domain_func("slot", &[a, i], ast.ref_type(), false, "IArray");
    let len_f = ast.domain_func("len", &[a], ast.int_type(), false, "IArray");
    let first_f = ast.domain_func("first", &[r], iarray_type, false, "IArray");
    let second_f = ast.domain_func("second", &[r], ast.int_type(), false, "IArray");
    let functions = [slot_f, len_f, first_f, second_f];

    let slot_a_i_app = ast.domain_func_app(
        slot_f,
        &[
            ast.local_var("a", iarray_type),
            ast.local_var("i", ast.int_type()),
        ],
        &[],
    );

    let first_app = ast.domain_func_app(first_f, &[slot_a_i_app], &[]);
    let second_app = ast.domain_func_app(second_f, &[slot_a_i_app], &[]);
    let len_app = ast.domain_func_app(len_f, &[ast.local_var("a", iarray_type)], &[]);

    let all_diff_ax = ast.named_domain_axiom(
        "all_diff",
        ast.forall(
            &[a, i],
            &[ast.trigger(&[slot_a_i_app])],
            ast.and(
                ast.eq_cmp(first_app, ast.local_var("a", iarray_type)),
                ast.eq_cmp(second_app, ast.local_var("i", ast.int_type())),
            ),
        ),
        "IArray",
    );

    let len_nonneg_ax = ast.named_domain_axiom(
        "len_nonneg",
        ast.forall(
            &[a],
            &[ast.trigger(&[len_app])],
            ast.ge_cmp(len_app, ast.int_lit(0)),
        ),
        "IArray",
    );

    let axioms = [all_diff_ax, len_nonneg_ax];

    ast.domain("IArray", &functions, &axioms, &[])
}

fn create_array_method(ast: AstFactory) -> Method {
    let iarray_type = ast.domain_type("IArray", &[], &[]);
    let int_type = ast.int_type();
    let l = ast.local_var("l", int_type);
    let h = ast.local_var("h", int_type);
    let src = ast.local_var("src", iarray_type);
    let dst = ast.local_var("dst", iarray_type);

    let args = [
        ast.local_var_decl("src", iarray_type),
        ast.local_var_decl("l", int_type),
        ast.local_var_decl("h", int_type),
    ];
    let returns = [ast.local_var_decl("dst", iarray_type)];

    let pres = [
        ast.le_cmp(ast.int_lit(0), l),
        ast.le_cmp(l, h),
        ast.lt_cmp(
            h,
            ast.domain_func_app2("len", &[src], &[], int_type, "IArray", ast.no_position()),
        ),
    ];

    // FIXME: finish preconditions and postconditions
    ast.method("copy_slice", &args, &returns, &pres, &[], None)
}
