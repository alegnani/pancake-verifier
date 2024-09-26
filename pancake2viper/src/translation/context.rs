use std::collections::HashMap;

use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::{ir::AnnotationType, pancake::Shape, viper_prelude::IArrayHelper};

use super::mangler::Mangler;

#[derive(Debug, Clone, Copy, Default)]
pub enum TranslationMode {
    #[default]
    Normal,
    PrePost,
    Assertion,
}

impl TranslationMode {
    pub fn is_annot(&self) -> bool {
        match self {
            Self::Normal => false,
            Self::Assertion | Self::PrePost => true,
        }
    }
}

impl From<AnnotationType> for TranslationMode {
    fn from(value: AnnotationType) -> Self {
        use AnnotationType::*;
        match value {
            Postcondition | Precondition => Self::PrePost,
            _ => Self::Assertion,
        }
    }
}

pub struct ViperEncodeCtx<'a> {
    mode: TranslationMode,
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    type_map: HashMap<String, Shape>,
    while_counter: u64,
    pub iarray: IArrayHelper<'a>,
    pub options: EncodeOptions,
    pub mangler: Mangler,

    pub pres: Vec<viper::Expr<'a>>,
    pub posts: Vec<viper::Expr<'a>>,
    pub invariants: Vec<viper::Expr<'a>>,
}

#[derive(Clone, Copy)]
pub struct EncodeOptions {
    pub expr_unrolling: bool,
    pub assert_aligned_accesses: bool,
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            expr_unrolling: false,
            assert_aligned_accesses: true,
        }
    }
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(fname: String, ast: AstFactory<'a>, options: EncodeOptions) -> Self {
        let mut type_map = HashMap::new();
        // FIXME: correctly set retval
        type_map.insert("heap".into(), Shape::Simple);
        type_map.insert("retval".into(), Shape::Simple);
        Self {
            mode: TranslationMode::Normal,
            ast,
            stack: vec![],
            declarations: vec![],
            type_map,
            while_counter: 0,
            iarray: IArrayHelper::new(ast),
            options,
            mangler: Mangler::new(fname),
            pres: vec![],
            posts: vec![],
            invariants: vec![],
        }
    }

    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            ast: self.ast,
            stack: vec![],
            declarations: vec![],
            type_map: self.type_map.clone(),
            while_counter: self.while_counter,
            iarray: self.iarray,
            options: self.options,
            mangler: self.mangler.child(),
            pres: self.pres.clone(),
            posts: self.posts.clone(),
            invariants: self.invariants.clone(),
        }
    }

    pub fn fresh_var(&mut self) -> String {
        self.mangler.fresh_var()
    }

    pub fn current_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter)
    }

    pub fn current_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter)
    }

    pub fn return_label(&self) -> &'static str {
        "return_label"
    }

    fn return_var_name(&self) -> &'static str {
        "retval"
    }

    pub fn return_var(&self) -> (viper::LocalVarDecl, viper::Expr) {
        (
            self.ast
                .local_var_decl(self.return_var_name(), self.ast.int_type()),
            self.ast
                .local_var(self.return_var_name(), self.ast.int_type()),
        )
    }

    pub fn new_while_ctx(&mut self) {
        self.while_counter += 1;
    }

    pub fn pop_decls(&mut self) -> Vec<Declaration<'a>> {
        self.declarations
            .drain(..)
            .map(LocalVarDecl::into)
            .collect()
    }

    pub fn heap_type(&self) -> viper::Type {
        self.iarray.get_type()
    }

    pub fn get_type(&self, var: &str) -> Shape {
        self.type_map
            .get(self.mangler.mangle_var(var))
            .unwrap_or_else(|| panic!("Type for '{}' has not been set", var))
            .to_owned()
    }

    pub fn set_type(&mut self, var: String, shape: Shape) {
        self.type_map.insert(var, shape);
    }

    pub fn heap_var(&self) -> (viper::LocalVarDecl, viper::Expr) {
        (
            self.ast.local_var_decl("heap", self.heap_type()),
            self.ast.local_var("heap", self.heap_type()),
        )
    }

    pub fn set_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
        self.mangler.mangle_mode(mode);
    }

    pub fn get_mode(&self) -> TranslationMode {
        self.mode
    }
}