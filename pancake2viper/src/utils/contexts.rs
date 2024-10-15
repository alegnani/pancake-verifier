use std::collections::{HashMap, HashSet};

use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::{ir::AnnotationType, viper_prelude::IArrayHelper};

use super::{
    mangler::{Mangler, RESERVED},
    Shape, TranslationError,
};

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

#[derive(Debug, Clone)]
pub struct TypeContext {
    type_map: HashMap<String, Shape>,
}

impl TypeContext {
    pub fn new() -> Self {
        let mut type_map = HashMap::new();
        for &keyword in RESERVED.iter() {
            type_map.insert(keyword.into(), Shape::Simple);
        }
        type_map.insert("heap".into(), Shape::Nested(vec![])); // FIXME hack to have heap be of type IArray
        Self { type_map }
    }

    pub fn child(&self) -> Self {
        self.clone()
    }

    pub fn get_type_no_mangle(&self, var: &str) -> Result<Shape, TranslationError> {
        self.type_map
            .get(var)
            .cloned()
            .ok_or(TranslationError::UnknownShape(var.to_owned()))
    }

    pub fn get_function_type(&self, fname: &str) -> Result<Shape, TranslationError> {
        self.type_map
            .get(fname)
            .cloned()
            .ok_or(TranslationError::UnknownReturnType(fname.to_owned()))
    }

    pub fn set_type(&mut self, var: String, shape: Shape) {
        self.type_map.insert(var, shape);
    }

    pub fn size(&self) -> usize {
        self.type_map.len()
    }
}

pub struct ViperEncodeCtx<'a> {
    mode: TranslationMode,
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    while_counter: u64,
    types: TypeContext,
    pub iarray: IArrayHelper<'a>,
    pub options: EncodeOptions,

    pub pres: Vec<viper::Expr<'a>>,
    pub posts: Vec<viper::Expr<'a>>,
    pub invariants: Vec<viper::Expr<'a>>,
    predicates: HashSet<String>,
    pub mangler: Mangler,
}

#[derive(Clone, Copy)]
pub struct EncodeOptions {
    pub expr_unrolling: bool,
    pub assert_aligned_accesses: bool,
    pub word_size: u64,
    pub heap_size: u64,
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            expr_unrolling: false,
            assert_aligned_accesses: true,
            word_size: 64,
            heap_size: 16 * 1024,
        }
    }
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(
        types: TypeContext,
        predicates: HashSet<String>,
        ast: AstFactory<'a>,
        options: EncodeOptions,
    ) -> Self {
        Self {
            mode: TranslationMode::Normal,
            ast,
            stack: vec![],
            declarations: vec![],
            types,
            while_counter: 0,
            iarray: IArrayHelper::new(ast),
            options,
            pres: vec![],
            posts: vec![],
            invariants: vec![],
            predicates,
            mangler: Mangler::default(),
        }
    }

    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            ast: self.ast,
            stack: vec![],
            declarations: vec![],
            types: self.types.child(),
            while_counter: self.while_counter,
            iarray: self.iarray,
            options: self.options,
            pres: self.pres.clone(),
            posts: self.posts.clone(),
            invariants: self.invariants.clone(),
            predicates: self.predicates.clone(),
            mangler: self.mangler.clone(),
        }
    }

    pub fn enter_new_loop(&mut self) {
        self.while_counter += 1;
    }

    pub fn current_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter)
    }

    pub fn current_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter)
    }

    pub fn outer_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter - 1)
    }

    pub fn outer_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter - 1)
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

    pub fn heap_var(&self) -> (viper::LocalVarDecl, viper::Expr) {
        (
            self.ast.local_var_decl("heap", self.heap_type()),
            self.ast.local_var("heap", self.heap_type()),
        )
    }

    pub fn set_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
    }

    pub fn get_mode(&self) -> TranslationMode {
        self.mode
    }

    pub fn is_predicate(&self, ident: &str) -> bool {
        self.predicates.contains(ident)
    }

    pub fn get_type(&self, var: &str) -> Result<Shape, TranslationError> {
        self.types.get_type_no_mangle(var)
    }

    pub fn get_function_type(&self, fname: &str) -> Result<Shape, TranslationError> {
        self.types.get_function_type(fname)
    }

    pub fn set_type(&mut self, var: String, shape: Shape) {
        self.types.set_type(var, shape);
    }

    pub fn type_ctx(&self) -> &TypeContext {
        &self.types
    }

    pub fn typectx_get(&self) -> &TypeContext {
        &self.types
    }

    pub fn typectx_get_mut(&mut self) -> &mut TypeContext {
        &mut self.types
    }
}
