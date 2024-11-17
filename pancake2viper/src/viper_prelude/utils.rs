use viper::{AstFactory, Expr, Function};

use crate::utils::{EncodeOptions, ViperUtils};

pub fn bound_bits_function(ast: AstFactory, bits: u64) -> Function {
    let x = ast.new_var("x", ast.int_type());
    let body = ast.and(
        ast.le_cmp(ast.zero(), x.1),
        ast.lt_cmp(
            x.1,
            ast.mul(ast.int_lit(4), ast.int_lit(2i64.pow(bits as u32 - 2))),
        ),
    );
    ast.function(
        &format!("bounded{}", bits),
        &[x.0],
        ast.bool_type(),
        &[],
        &[],
        ast.no_position(),
        Some(body),
    )
}

pub fn bound_function<'a>(
    ast: AstFactory<'a>,
    utils: &Utils,
    options: EncodeOptions,
) -> Function<'a> {
    let x = ast.new_var("x", ast.int_type());
    ast.function(
        "bounded",
        &[x.0],
        ast.bool_type(),
        &[],
        &[],
        ast.no_position(),
        Some(utils.bounded_f(x.1, options.word_size)),
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

    pub fn bounded_f(&self, var: Expr, bits: u64) -> Expr<'a> {
        self.ast.func_app(
            &format!("bounded{}", bits),
            &[var],
            self.ast.bool_type(),
            self.ast.no_position(),
        )
    }
}
