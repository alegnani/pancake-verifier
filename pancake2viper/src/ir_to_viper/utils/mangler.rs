use std::{
    collections::{HashMap, HashSet},
    sync::{LazyLock, Mutex},
};

use super::context::TranslationMode;

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
    fname: String,
    args: HashMap<String, String>,
    annot_vars: HashSet<String>,
    var_map: HashMap<String, String>,
}

impl Mangler {
    pub fn new(fname: String) -> Self {
        Self {
            fname,
            ..Default::default()
        }
    }

    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            fname: self.fname.clone(),
            args: self.args.clone(),
            annot_vars: self.annot_vars.clone(),
            var_map: self.var_map.clone(),
        }
    }

    pub fn new_scoped_var(&mut self, var: String) -> String {
        if RESERVED.contains(var.as_str()) {
            panic!(
                "'{}' is a reserved keyword and can't be used as an identifier",
                &var
            );
        }
        let mangled = format!("{}_{}_{}", &self.fname, &var, get_inc_counter());
        self.var_map.insert(var, mangled.clone());
        mangled
    }

    pub fn new_annot_var(&mut self, var: String) {
        assert!(
            self.annot_vars.insert(var.clone()),
            "Duplicated variable in annotation: '{}'\n{:?}",
            var,
            self
        );
    }

    pub fn clear_annot_var(&mut self) {
        self.annot_vars.clear();
    }

    pub fn new_arg(&mut self, arg: String) -> String {
        let mangled = Self::mangle_arg(&arg);
        if self.args.insert(arg.clone(), mangled.clone()).is_some() {
            panic!("Arg '{}' was already defined\n{:?}", &arg, self);
        }
        mangled
    }

    pub fn fresh_var(&mut self) -> String {
        let fresh = format!("fr_{}", get_inc_counter());
        fresh
    }

    pub fn mangle_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
    }

    pub fn mangle_var<'a>(&'a self, var: &'a str) -> &'a str {
        if let TranslationMode::PrePost = self.mode {
            if let Some(ret) = self.args.get(var) {
                return ret;
            }
        }

        if let TranslationMode::PrePost | TranslationMode::Assertion = self.mode {
            if RESERVED.contains(var) {
                return var;
            }
            if let Some(ret) = self.annot_vars.get(var) {
                return ret;
            }
        }
        self.var_map
            .get(var)
            .unwrap_or_else(|| panic!("Variable '{}' has not been defined\n{:?}", var, self))
    }

    pub fn mangle_arg(arg: &str) -> String {
        format!("arg_{}", arg)
    }

    pub fn mangle_fn(&self, fname: &str) -> String {
        format!("f_{}", fname)
    }
}
