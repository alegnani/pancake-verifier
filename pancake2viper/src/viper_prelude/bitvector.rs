use viper::{AstFactory, Domain, DomainFunc};

pub fn create_bv_domain(ast: AstFactory) -> Domain {
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

    let not = ast.domain_func_with_interpretation(
        "not",
        &[ast.local_var_decl("a", bv64)],
        bv64,
        false,
        "BitVectorDomain64",
        Some("bvnot".into()),
    );

    ast.domain_with_interpretation(
        "BitVectorDomain64",
        &[
            not,
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
