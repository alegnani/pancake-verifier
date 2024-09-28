use crate::{ir, pancake};

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

impl From<pancake::Program> for ir::Program {
    fn from(value: pancake::Program) -> Self {
        let functions: Wrapper<ir::FnDec> = value.functions.into();
        Self {
            functions: functions.0,
        }
    }
}
