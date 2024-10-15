use crate::{
    annotation::{parse_function, parse_method, parse_predicate},
    ir, pancake,
    utils::{Mangler, TranslationError, TryToIR, VariableType},
};

impl From<pancake::Arg> for ir::Arg {
    fn from(value: pancake::Arg) -> Self {
        Self {
            name: value.name,
            shape: value.shape,
        }
    }
}

impl TryToIR for pancake::Arg {
    type Output = ir::Arg;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            name: mangler.new_mangled_var(self.name, VariableType::Argument)?,
            shape: self.shape,
        })
    }
}

impl TryToIR for pancake::FnDec {
    type Output = ir::FnDec;

    fn to_ir(self, mangler: &mut Mangler) -> Result<Self::Output, TranslationError> {
        mangler.set_fname(self.fname.clone());
        let args = self.args.to_ir(mangler)?;
        Ok(Self::Output {
            fname: self.fname,
            args,
            body: self.body.to_ir(mangler)?,
        })
    }
}

impl TryToIR for pancake::Predicate {
    type Output = ir::Predicate;

    fn to_ir(self, _ctx: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(parse_predicate(&self.text))
    }
}

impl TryToIR for pancake::Function {
    type Output = ir::Function;

    fn to_ir(self, _ctx: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(parse_function(&self.text))
    }
}

impl TryToIR for pancake::Method {
    type Output = ir::AbstractMethod;

    fn to_ir(self, _ctx: &mut Mangler) -> Result<Self::Output, TranslationError> {
        Ok(parse_method(&self.text))
    }
}

impl TryFrom<pancake::Program> for ir::Program {
    type Error = TranslationError;

    fn try_from(value: pancake::Program) -> Result<Self, Self::Error> {
        let mut mangler = Mangler::default();
        let viper_functions = value.viper_functions.to_ir(&mut mangler)?;
        let predicates = value.predicates.to_ir(&mut mangler)?;
        let functions = value.functions.to_ir(&mut mangler)?;
        let methods = value.methods.to_ir(&mut mangler)?;

        Ok(ir::Program {
            functions,
            predicates,
            viper_functions,
            methods,
        })
    }
}
