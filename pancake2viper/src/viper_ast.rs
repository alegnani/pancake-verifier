use std::fmt::Display;

type ViperFieldName = String;
type ViperVarName = String;
type ViperPredName = String;

pub struct ViperArgDecl(pub ViperVarName, pub ViperType);

pub enum ViperExpr {
    IntLit(i64),
    BoolLit(bool),
    NullLit,
    Var(ViperVarName),
    BinaryOp(Box<ViperExpr>, ViperOp, Box<ViperExpr>),
    UnaryOp(ViperOp, Box<ViperExpr>),
}

pub enum ViperStmt {
    VarDecl(ViperVarName, ViperType),
    VarAssign(ViperVarName, ViperExpr),
    FieldAssign(ViperExpr, ViperFieldName, ViperExpr),
    MethodCall(Vec<ViperVarName>, String, Vec<ViperExpr>),
    Assert(ViperExpr),
    Assume(ViperExpr),
    Seq(Vec<ViperStmt>), // XXX: needed?
    If(ViperExpr, Box<ViperStmt>, Box<ViperStmt>),
    While(ViperExpr, Vec<ViperExpr>, Box<ViperStmt>),
    Label(String),
    Goto(String),
    Skip,
}

pub enum ViperMember {
    Field(ViperFieldName, ViperType),
    Method(ViperMethod),
}

pub struct ViperMethod {
    pub name: String,
    pub args: Vec<ViperArgDecl>,
    pub returns: Vec<ViperArgDecl>,
    pub pre: Vec<ViperExpr>,
    pub post: Vec<ViperExpr>,
    pub body: Option<ViperStmt>,
}

pub enum ViperOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    Implies,
    And,
    Or,
}

pub enum ViperType {
    Int,
    Bool,
}

fn indent_block(block: &str) -> String {
    block
        .split_inclusive('\n')
        .map(|l| format!("\t{}", l))
        .collect::<Vec<_>>()
        .join("")
}

fn sep_list(strs: &[impl Display], separator: &str) -> String {
    strs.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(separator)
}

fn comma_sep(strs: &[impl Display]) -> String {
    sep_list(strs, ", ")
}

// Display trait implementations for Viper AST

impl Display for ViperType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Bool => "Bool",
            Self::Int => "Int",
        };
        write!(f, "{}", s)
    }
}

impl Display for ViperArgDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

impl Display for ViperOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Implies => "==>",
            Self::And => "&&",
            Self::Or => "||",
        };
        write!(f, "{}", s)
    }
}

impl Display for ViperExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::BinaryOp(a, op, b) => {
                format!("{} {} {}", a, op, b)
            }
            Self::BoolLit(true) => "true".into(),
            Self::BoolLit(false) => "false".into(),
            Self::IntLit(x) => format!("{}", x),
            Self::NullLit => "null".into(),
            Self::UnaryOp(op, a) => format!("{}{}", op, a),
            Self::Var(name) => name.clone(),
        };
        write!(f, "{}", s)
    }
}

impl Display for ViperStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Assert(expr) => format!("assert {}", expr),
            Self::Assume(expr) => format!("assume {}", expr),
            Self::FieldAssign(lhs, name, rhs) => todo!(),
            Self::Goto(label) => format!("goto {}", label),
            Self::If(cond, ifb, elseb) => format!(
                "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                cond,
                indent_block(&ifb.to_string()),
                indent_block(&elseb.to_string())
            ),
            Self::Label(label) => format!("label {}", label),
            Self::MethodCall(rets, name, args) if rets.is_empty() => {
                format!("{}({})", name, comma_sep(args))
            }
            Self::MethodCall(rets, name, args) => {
                format!("{} := {}({})", rets.join(", "), name, comma_sep(args))
            }
            Self::Seq(seq) => sep_list(seq, "\n"),
            Self::Skip => "".into(),
            Self::VarAssign(name, expr) => format!("{} := {}", name, expr),
            Self::VarDecl(name, typ) => format!("var {}: {}", name, typ),
            Self::While(cond, invs, body) if invs.is_empty() => {
                format!("while ({}) {{\n{}\n}}", cond, body)
            }
            Self::While(cond, invs, body) => {
                format!(
                    "while ({})\n{}\n{{\n{}\n}}",
                    cond,
                    indent_block(&sep_list(invs, "\n")),
                    body
                )
            }
        };
        write!(f, "{}", s)
    }
}

impl Display for ViperMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Field(name, typ) => todo!(),
            Self::Method(m) => {
                let args = m.args.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                let returns = m.returns.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                let pre = m
                    .pre
                    .iter()
                    .map(|e| format!("requires {}", e))
                    .collect::<Vec<_>>()
                    .join("\n");
                let post = m
                    .post
                    .iter()
                    .map(|e| format!("ensures {}", e))
                    .collect::<Vec<_>>()
                    .join("\n");
                let body = match &m.body {
                    None => "".into(),
                    Some(b) => format!("{{\n{}\n}}", indent_block(&b.to_string())),
                };
                format!(
                    "method {}({}) returns ({})\n{}\n{}\n{}\n",
                    m.name,
                    args.join("\n"),
                    returns.join("\n"),
                    indent_block(&pre),
                    indent_block(&post),
                    body
                )
            }
        };
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn method_test() {
        let method = ViperMember::Method(ViperMethod {
            name: "test".into(),
            args: vec![ViperArgDecl("arg1".into(), ViperType::Int)],
            returns: vec![ViperArgDecl("value".into(), ViperType::Int)],
            pre: vec![
                ViperExpr::BinaryOp(
                    Box::new(ViperExpr::Var("arg1".into())),
                    ViperOp::Gte,
                    Box::new(ViperExpr::IntLit(0)),
                ),
                ViperExpr::BinaryOp(
                    Box::new(ViperExpr::Var("arg1".into())),
                    ViperOp::Lt,
                    Box::new(ViperExpr::IntLit(64)),
                ),
            ],
            post: vec![ViperExpr::BinaryOp(
                Box::new(ViperExpr::Var("arg1".into())),
                ViperOp::Lte,
                Box::new(ViperExpr::IntLit(74)),
            )],
            body: Some(ViperStmt::VarAssign(
                "value".into(),
                ViperExpr::BinaryOp(
                    Box::new(ViperExpr::Var("arg1".into())),
                    ViperOp::Add,
                    Box::new(ViperExpr::IntLit(10)),
                ),
            )),
        });
        println!("{}", method);
    }
}
