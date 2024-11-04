use crate::{
    annotation::{parse_function, parse_method, parse_predicate},
    ir, pancake,
    utils::{ToType, TranslationError, TryToIR},
};

use super::utils::stmt_annotation_push;

impl TryToIR for pancake::Arg {
    type Output = ir::Arg;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            name: self.name,
            typ: self.shape.to_type(),
        })
    }
}

impl TryToIR for pancake::FnDec {
    type Output = ir::FnDec;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        let mut body = args.iter().fold(self.body.to_ir()?, |scope, arg| {
            ir::Stmt::Definition(ir::Definition {
                lhs: arg.name.clone(),
                rhs: ir::Expr::Var(arg.name.clone()),
                scope: Box::new(scope),
            })
        });
        let (pres, posts) = stmt_annotation_push(&mut body);
        Ok(Self::Output {
            fname: self.fname,
            args,
            body,
            pres,
            posts,
            retvar: "retval".into(),
        })
    }
}

impl TryToIR for pancake::Predicate {
    type Output = ir::Predicate;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(parse_predicate(&self.text))
    }
}

impl TryToIR for pancake::Function {
    type Output = ir::Function;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(parse_function(&self.text))
    }
}

impl TryToIR for pancake::Method {
    type Output = ir::AbstractMethod;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(parse_method(&self.text))
    }
}

impl TryFrom<pancake::Program> for ir::Program {
    type Error = TranslationError;

    fn try_from(value: pancake::Program) -> Result<Self, Self::Error> {
        let viper_functions = value.viper_functions.to_ir()?;
        let predicates = value.predicates.to_ir()?;
        let functions = value.functions.to_ir()?;
        let methods = value.methods.to_ir()?;

        Ok(ir::Program {
            functions,
            predicates,
            viper_functions,
            methods,
        })
    }
}
