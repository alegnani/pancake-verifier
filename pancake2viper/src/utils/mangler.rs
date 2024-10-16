use std::{
    collections::{HashMap, HashSet},
    sync::{LazyLock, Mutex},
};

use super::{MangleError, TranslationMode};

lazy_static::lazy_static! {
    pub static ref RESERVED: HashSet<&'static str> = HashSet::from(["heap", "retval", "read", "write", "wildcard", "acc", "alen"]);
}

static COUNTER: LazyLock<Mutex<u64>> = LazyLock::new(|| Mutex::new(0));

fn get_inc_counter() -> u64 {
    let mut c = COUNTER.lock().unwrap();
    let ret = *c;
    *c += 1;
    ret
}

#[derive(Debug, Clone, Default)]
pub struct Mangler {
    mode: TranslationMode,
    fname: Option<String>,
    annot_map: HashMap<String, String>,
    var_map: HashMap<String, String>,
    arg_map: HashMap<String, String>,
}

pub enum VariableType {
    Variable,
    Argument,
}

impl Mangler {
    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            fname: self.fname.clone(),
            annot_map: self.annot_map.clone(),
            var_map: self.var_map.clone(),
            arg_map: self.arg_map.clone(),
        }
    }

    pub fn new_mangled_var(
        &mut self,
        name: String,
        typ: VariableType,
    ) -> Result<String, MangleError> {
        if RESERVED.contains(name.as_str()) {
            return Err(MangleError::ReservedKeyword(name));
        }
        let mangled = format!("{}_{}_{}", self.get_fname(), &name, get_inc_counter());
        let map = match (&typ, self.mode) {
            (VariableType::Variable, TranslationMode::Normal) => &mut self.var_map,
            (VariableType::Variable, _) => &mut self.annot_map,
            (VariableType::Argument, TranslationMode::Normal) => &mut self.arg_map,
            _ => unreachable!(),
        };
        // Check if variable has already been declared. For normal variables
        // this is allowed due to shadowing.
        if map.contains_key(&name)
            && !matches!(
                (typ, self.mode),
                (VariableType::Variable, TranslationMode::Normal)
            )
        {
            return Err(MangleError::DoubleDeclaration(name));
        }
        map.insert(name.clone(), mangled.clone());
        Ok(mangled)
    }

    pub fn clear_annot_var(&mut self) {
        self.annot_map.clear();
    }

    pub fn fresh_varname() -> String {
        let fresh = format!("fr_{}", get_inc_counter());
        fresh
    }

    pub fn mangle_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
    }

    pub fn mangle_var<'a>(&'a self, var: &'a str) -> Result<&'a str, MangleError> {
        if RESERVED.contains(var) {
            return Ok(var);
        }
        let maybe_arg = self.arg_map.get(var);
        let maybe_annot = self.annot_map.get(var);
        let maybe_var = self.var_map.get(var);
        match self.mode {
            TranslationMode::Normal => maybe_var.or(maybe_arg),
            TranslationMode::Assertion => maybe_annot.or(maybe_var),
            TranslationMode::PrePost => maybe_annot.or(maybe_arg),
        }
        .map(String::as_str)
        .ok_or(MangleError::UndeclaredVar(var.to_owned()))
    }

    fn get_fname(&self) -> &str {
        self.fname
            .as_ref()
            .expect("Mangler's current function name not set")
    }

    pub fn set_fname(&mut self, fname: String) {
        self.fname = Some(fname);
    }

    pub fn mangle_fn(fname: &str) -> String {
        format!("f_{}", fname)
    }

    // pub fn demangle_fn(mangled:& str) -> String {

    // }
}
