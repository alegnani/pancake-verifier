use viper::{AstFactory, Expr, Function};

use crate::utils::ViperUtils;

pub fn bound_function(ast: AstFactory, word_size: u64) -> Function {
    let x = ast.new_var("x", ast.int_type());
    let body = ast.and(
        ast.le_cmp(ast.zero(), x.1),
        ast.lt_cmp(
            x.1,
            ast.mul(ast.int_lit(4), ast.int_lit(2i64.pow(word_size as u32 - 2))),
        ),
    );
    ast.function(
        "bounded",
        &[x.0],
        ast.bool_type(),
        &[],
        &[],
        ast.no_position(),
        Some(body),
    )
}

pub fn pow_function(ast: AstFactory) -> Function {
    let (e_decl, e) = ast.new_var("e", ast.int_type());
    let body = ast.cond_exp(
        ast.eq_cmp(e, ast.zero()),
        ast.one(),
        ast.mul(
            ast.two(),
            ast.func_app(
                "pow",
                &[ast.sub(e, ast.one())],
                ast.int_type(),
                ast.no_position(),
            ),
        ),
    );
    ast.function(
        "pow",
        &[e_decl],
        ast.int_type(),
        &[ast.ge_cmp(e, ast.zero())],
        &[],
        ast.no_position(),
        Some(body),
    )
}

#[derive(Clone, Copy)]
pub struct Utils<'a> {
    ast: AstFactory<'a>,
}

impl<'a> Utils<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self { ast }
    }

    pub fn bounded_f(&self, var: Expr) -> Expr<'a> {
        self.ast.func_app(
            "bounded",
            &[var],
            self.ast.bool_type(),
            self.ast.no_position(),
        )
    }
}
