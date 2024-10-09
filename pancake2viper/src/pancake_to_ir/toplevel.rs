use crate::{
    annotation::{parse_function, parse_method, parse_predicate},
    ir, pancake,
};

use super::utils::Wrapper;

impl From<pancake::Arg> for ir::Arg {
    fn from(value: pancake::Arg) -> Self {
        Self {
            name: value.name,
            shape: value.shape,
        }
    }
}

impl From<pancake::FnDec> for ir::FnDec {
    fn from(value: pancake::FnDec) -> Self {
        let args: Wrapper<ir::Arg> = value.args.into();
        Self {
            fname: value.fname,
            args: args.0,
            body: value.body.into(),
        }
    }
}

impl From<pancake::Predicate> for ir::Predicate {
    fn from(value: pancake::Predicate) -> Self {
        parse_predicate(&value.text)
    }
}

impl From<pancake::Function> for ir::Function {
    fn from(value: pancake::Function) -> Self {
        parse_function(&value.text)
    }
}

impl From<pancake::Method> for ir::AbstractMethod {
    fn from(value: pancake::Method) -> Self {
        parse_method(&value.text)
    }
}

impl From<pancake::Program> for ir::Program {
    fn from(value: pancake::Program) -> Self {
        let functions: Wrapper<ir::FnDec> = value.functions.into();
        // let predicates: Wrapper<ir::Predicate> = value.predicates.into(); /// XXX: wtf?
        let predicates = value.predicates.into_iter().map(|e| e.into()).collect();
        let viper_functions: Vec<_> = value
            .viper_functions
            .into_iter()
            .map(|e| e.into())
            .collect();
        let methods: Vec<_> = value.methods.into_iter().map(|e| e.into()).collect();
        Self {
            functions: functions.0,
            predicates,
            viper_functions,
            methods,
        }
    }
}
