use crate::{
    annotation::{parse_function, parse_method, parse_predicate},
    ir, pancake,
    utils::{ToIR, ToIRError, TryToIR, TypeContext},
};

impl From<pancake::Arg> for ir::Arg {
    fn from(value: pancake::Arg) -> Self {
        Self {
            name: value.name,
            shape: value.shape,
        }
    }
}

impl ToIR for pancake::Arg {
    type Output = ir::Arg;

    fn to_ir(self, ctx: &mut crate::utils::TypeContext) -> Self::Output {
        Self::Output {
            name: ctx.mangler_get_mut().new_arg(self.name),
            shape: self.shape,
        }
    }
}

impl TryToIR for pancake::FnDec {
    type Output = ir::FnDec;

    fn to_ir(self, ctx: &mut crate::utils::TypeContext) -> Result<Self::Output, ToIRError> {
        let args = self.args.to_ir(ctx);
        Ok(Self::Output {
            fname: self.fname,
            args,
            body: self.body.to_ir(ctx)?,
        })
    }
}

impl TryToIR for pancake::Predicate {
    type Output = ir::Predicate;

    fn to_ir(self, _ctx: &mut crate::utils::TypeContext) -> Result<Self::Output, ToIRError> {
        Ok(parse_predicate(&self.text))
    }
}

impl TryToIR for pancake::Function {
    type Output = ir::Function;

    fn to_ir(self, _ctx: &mut crate::utils::TypeContext) -> Result<Self::Output, ToIRError> {
        Ok(parse_function(&self.text))
    }
}

impl TryToIR for pancake::Method {
    type Output = ir::AbstractMethod;

    fn to_ir(self, _ctx: &mut crate::utils::TypeContext) -> Result<Self::Output, ToIRError> {
        Ok(parse_method(&self.text))
    }
}

impl TryFrom<pancake::Program> for ir::Program {
    type Error = ToIRError;

    fn try_from(value: pancake::Program) -> Result<Self, Self::Error> {
        let mut ctx = TypeContext::new();
        let viper_functions = value.viper_functions.to_ir(&mut ctx)?;
        let predicates = value.predicates.to_ir(&mut ctx)?;
        let functions = value.functions.to_ir(&mut ctx)?;
        let methods = value.methods.to_ir(&mut ctx)?;

        Ok(ir::Program {
            functions,
            predicates,
            viper_functions,
            methods,
        })
    }
}
