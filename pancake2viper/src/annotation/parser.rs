use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;

use crate::ir::*;
use crate::utils::Shape;

#[derive(pest_derive::Parser)]
#[grammar = "annotation/annot.pest"]
struct AnnotParser;

type ParseResult<T> = Result<T, Box<Error<Rule>>>;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        PrattParser::new()
            .op(Op::infix(Rule::imp, Right) | Op::infix(Rule::iff, Left))
            .op(Op::postfix(Rule::ternary))
            .op(Op::infix(Rule::bool_or, Left))
            .op(Op::infix(Rule::bool_and, Left))
            .op(Op::infix(Rule::bit_or, Left))
            .op(Op::infix(Rule::bit_xor, Left))
            .op(Op::infix(Rule::bit_and, Left))
            .op(Op::infix(Rule::pancake_eq, Left) | Op::infix(Rule::pancake_neq, Left) | Op::infix(Rule::viper_eq, Left) | Op::infix(Rule::viper_neq, Left))
            .op(Op::infix(Rule::gt, Left) | Op::infix(Rule::gte, Left) | Op::infix(Rule::lt, Left) | Op::infix(Rule::lte, Left))
            .op(Op::postfix(Rule::shift))
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
            .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left) | Op::infix(Rule::modulo, Left))
            .op(Op::prefix(Rule::neg) | Op::prefix(Rule::minus))
            .op(Op::postfix(Rule::field_acc))
            .op(Op::postfix(Rule::viper_field_acc))
            .op(Op::postfix(Rule::arr_acc))
    };
}

pub fn parse_annot(annot: &str, is_stmt: bool) -> ParseResult<Annotation> {
    let rule = if is_stmt {
        Rule::annotation_stmt
    } else {
        Rule::annotation
    };
    Ok(AnnotParser::parse(rule, annot).map(|mut pairs| {
        let mut pair = pairs.next().unwrap().into_inner();
        let typ = AnnotationType::from_pest(pair.next().unwrap());
        let expr = match typ {
            AnnotationType::Trusted => Expr::Const(1),
            _ => parse_expr(Pairs::single(pair.next().unwrap())),
        };
        Annotation { typ, expr }
    })?)
}

pub fn parse_shared(shared: &str) -> ParseResult<Shared> {
    Ok(
        AnnotParser::parse(Rule::shared_prototype, shared).map(|mut pairs| {
            let mut pair = pairs.next().unwrap().into_inner();
            let typ = SharedPerm::from_pest(pair.next().unwrap());
            let bits = pair
                .next()
                .unwrap()
                .as_str()
                .trim_start_matches('u')
                .parse()
                .unwrap();
            let name = pair.next().unwrap().as_str().to_owned();
            let addr = pair.next().unwrap();
            let (lower, upper, stride) = match addr.as_rule() {
                Rule::expr => {
                    let lower = parse_expr(Pairs::single(addr));
                    let upper = lower.clone() + 1;
                    (lower, upper, Expr::Const(bits as i64 / 8))
                }
                Rule::shared_range => {
                    let mut inner = addr.into_inner();
                    let lower = parse_expr(Pairs::single(inner.next().unwrap()));
                    let upper = parse_expr(Pairs::single(inner.next().unwrap()));
                    (lower, upper, Expr::Const(bits as i64 / 8))
                }
                Rule::shared_stride => {
                    let mut inner = addr.into_inner();
                    let lower = parse_expr(Pairs::single(inner.next().unwrap()));
                    let upper = parse_expr(Pairs::single(inner.next().unwrap()));
                    let stride = parse_expr(Pairs::single(inner.next().unwrap()));
                    (lower, upper, stride)
                }
                _ => unreachable!(),
            };
            Shared {
                name,
                typ,
                bits,
                lower,
                upper,
                stride,
            }
        })?,
    )
}

pub fn parse_predicate(pred: &str) -> ParseResult<Predicate> {
    let (name, args, mut pair) = parse_toplevel_common(pred, Rule::predicate)?;
    let body = pair.next().unwrap().into_inner();
    let body = if body.len() == 0 {
        None
    } else {
        Some(parse_expr(body))
    };
    Ok(Predicate {
        name,
        args: args.into_iter().map(Arg::from).collect(),
        body,
    })
}

pub fn parse_model_predicate(s: &str) -> ParseResult<Expr> {
    Ok(AnnotParser::parse(Rule::model_predicate, s)
        .map(|mut pairs| parse_expr(pairs.next().unwrap().into_inner()))?)
}

pub fn parse_model_field(s: &str) -> ParseResult<String> {
    Ok(AnnotParser::parse(Rule::model_field, s).map(|mut pairs| {
        pairs
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap()
            .as_str()
            .to_owned()
    })?)
}

pub fn parse_extern_predicate(s: &str) -> ParseResult<String> {
    Ok(AnnotParser::parse(Rule::ext_predicate, s).map(|mut pairs| {
        pairs
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap()
            .as_str()
            .to_owned()
    })?)
}

pub fn parse_extern_field(s: &str) -> ParseResult<Decl> {
    Ok(AnnotParser::parse(Rule::ext_field, s)
        .map(|mut pairs| Decl::from_pest(pairs.next().unwrap().into_inner().next().unwrap()))?)
}

pub fn parse_extern_ffi(s: &str) -> ParseResult<String> {
    Ok(AnnotParser::parse(Rule::ffi_method, s).map(|mut pairs| {
        pairs
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap()
            .as_str()
            .to_owned()
    })?)
}

fn partition_annotation_types(
    annotations: Vec<Annotation>,
) -> (Vec<Annotation>, Vec<Annotation>, Vec<Annotation>) {
    let (pres, others): (Vec<_>, Vec<_>) = annotations
        .into_iter()
        .partition(|e| matches!(e.typ, AnnotationType::Precondition));
    let (posts, others): (Vec<_>, Vec<_>) = others
        .into_iter()
        .partition(|e| matches!(e.typ, AnnotationType::Postcondition));
    (pres, posts, others)
}

pub fn parse_function(func: &str) -> ParseResult<Function> {
    let (name, args, mut pair) = parse_toplevel_common(func, Rule::function)?;
    let typ = Type::from_pest(pair.next().unwrap());

    let preposts = pair
        .next()
        .unwrap()
        .into_inner()
        .map(|e| parse_annot(e.as_str(), false))
        .collect::<Result<Vec<_>, _>>()?;
    let (pres, posts, others) = partition_annotation_types(preposts);
    if !others.is_empty() {
        panic!("Invalid annotation in Function pre-/post-condition");
    }
    let pres = pres.into_iter().map(|a| a.expr).collect();
    let posts = posts.into_iter().map(|a| a.expr).collect();

    let body = pair.next().unwrap().into_inner();
    let body = if body.len() == 0 {
        None
    } else {
        Some(parse_expr(body))
    };
    Ok(Function {
        name,
        args: args.into_iter().map(Arg::from).collect(),
        typ,
        pres,
        posts,
        body,
    })
}

pub fn parse_method(met: &str) -> ParseResult<AbstractMethod> {
    let (name, args, mut pair) = parse_toplevel_common(met, Rule::method)?;
    let rettyps = pair.next().unwrap().into_inner();
    let rettyps = rettyps.into_iter().map(|d| Decl::from_pest(d)).collect();
    let preposts = pair
        .next()
        .unwrap()
        .into_inner()
        .map(|e| parse_annot(e.as_str(), false))
        .collect::<ParseResult<Vec<_>>>()?;

    let (pres, posts, others) = partition_annotation_types(preposts);
    if !others.is_empty() {
        panic!("Invalid annotation in Function pre-/post-condition");
    }
    let pres = pres.into_iter().map(|a| a.expr).collect();
    let posts = posts.into_iter().map(|a| a.expr).collect();

    Ok(AbstractMethod {
        name,
        args: args.into_iter().map(Arg::from).collect(),
        rettyps,
        pres,
        posts,
    })
}

fn parse_toplevel_common(s: &str, rule: Rule) -> ParseResult<(String, Vec<Decl>, Pairs<Rule>)> {
    Ok(AnnotParser::parse(rule, s).map(|mut pairs| {
        let mut pair = pairs.next().unwrap().into_inner();
        let name = pair.next().unwrap().as_str().to_owned();
        let args = pair
            .next()
            .unwrap()
            .into_inner()
            .map(Decl::from_pest)
            .collect();
        (name, args, pair)
    })?)
}

fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::struc => Expr::Struct(Struct::from_pest(primary)),
            Rule::int_lit => Expr::Const(i64::from_pest(primary)),
            Rule::quantified => Expr::Quantified(Quantified::from_pest(primary)),
            Rule::expr => parse_expr(primary.into_inner()),
            Rule::ident => Expr::Var(primary.as_str().to_owned()),
            Rule::old => Expr::Old(Old {
                expr: Box::new(parse_expr(primary.into_inner())),
            }),
            Rule::f_call => Expr::FunctionCall(FunctionCall::from_pest(primary)),
            Rule::acc_slice => Expr::AccessSlice(AccessSlice::from_pest(primary)),
            Rule::acc_pred => Expr::AccessPredicate(AccessPredicate::from_pest(primary)),
            Rule::unfolding => Expr::UnfoldingIn(UnfoldingIn::from_pest(primary)),
            Rule::base => Expr::BaseAddr,
            Rule::biw => Expr::BytesInWord,
            Rule::true_lit => Expr::BoolLit(true),
            Rule::false_lit => Expr::BoolLit(false),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| {
            Expr::UnOp(UnOp {
                right: Box::new(rhs),
                optype: UnOpType::from_pest(op),
            })
        })
        .map_infix(|lhs, op, rhs| {
            Expr::BinOp(BinOp {
                optype: BinOpType::from_pest(op),
                left: Box::new(lhs),
                right: Box::new(rhs),
            })
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::field_acc => Expr::Field(Field {
                obj: Box::new(lhs),
                field_idx: op.into_inner().next().unwrap().as_str().parse().unwrap(),
            }),
            Rule::viper_field_acc => Expr::ViperFieldAccess(ViperFieldAccess {
                obj: Box::new(lhs),
                field: op.as_str().trim_start_matches('.').to_string(),
            }),
            Rule::arr_acc => Expr::ArrayAccess(ArrayAccess {
                obj: Box::new(lhs),
                idx: Box::new(parse_expr(op.into_inner())),
            }),
            Rule::ternary => {
                let mut pairs = op.into_inner();
                Expr::Ternary(Ternary {
                    cond: Box::new(lhs),
                    left: Box::new(parse_expr(Pairs::single(pairs.next().unwrap()))),
                    right: Box::new(parse_expr(Pairs::single(pairs.next().unwrap()))),
                })
            }
            Rule::shift => {
                let mut pairs = op.into_inner();
                Expr::Shift(Shift {
                    shifttype: ShiftType::from_pest(pairs.next().unwrap()),
                    value: Box::new(lhs),
                    amount: pairs.next().unwrap().as_str().parse().unwrap(),
                })
            }
            _ => unreachable!(),
        })
        .parse(pairs)
}

// Translates from parsing tree to AST
pub trait FromPestPair {
    fn from_pest(pair: Pair<'_, Rule>) -> Self;
}

impl FromPestPair for Struct {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let elements = pair
            .into_inner()
            .map(|p| parse_expr(Pairs::single(p)))
            .collect::<Vec<_>>();
        Self { elements }
    }
}

impl FromPestPair for i64 {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::decimal_lit => inner.as_str().replace("_", "").parse().unwrap(),
            Rule::hex_lit => {
                i64::from_str_radix(inner.as_str().replace("_", "").trim_start_matches("0x"), 16)
                    .unwrap()
            }
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for SharedPerm {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_str() {
            "rw" | "wr" => Self::ReadWrite,
            "r" => Self::ReadOnly,
            "w" => Self::WriteOnly,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for ShiftType {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::lshl => Self::Lsl,
            Rule::ashr => Self::Asr,
            Rule::lshr => Self::Lsr,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for AnnotationType {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::pre => Self::Precondition,
            Rule::post => Self::Postcondition,
            Rule::assertion => Self::Assertion,
            Rule::refutation => Self::Refutation,
            Rule::invariant => Self::Invariant,
            Rule::assumption => Self::Assumption,
            Rule::inhale => Self::Inhale,
            Rule::exhale => Self::Exhale,
            Rule::fold => Self::Fold,
            Rule::unfold => Self::Unfold,
            Rule::trusted => Self::Trusted,
            Rule::use_f => Self::Use,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for UnOpType {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::neg => Self::Neg,
            Rule::minus => Self::Minus,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for BinOpType {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::add => Self::Add,
            Rule::sub => Self::Sub,
            Rule::mul => Self::Mul,
            Rule::div => Self::Div,
            Rule::modulo => Self::Modulo,
            Rule::imp => Self::Imp,
            Rule::iff => Self::Iff,
            Rule::viper_eq => Self::ViperEqual,
            Rule::viper_neq => Self::ViperNotEqual,
            Rule::pancake_eq => Self::PancakeEqual,
            Rule::pancake_neq => Self::PancakeNotEqual,
            Rule::gt => Self::Gt,
            Rule::gte => Self::Gte,
            Rule::lt => Self::Lt,
            Rule::lte => Self::Lte,
            Rule::bool_and => Self::BoolAnd,
            Rule::bool_or => Self::BoolOr,
            Rule::bit_and => Self::BitAnd,
            Rule::bit_or => Self::BitOr,
            Rule::bit_xor => Self::BitXor,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for Type {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::bool_t => Self::Bool,
            Rule::int_t => Self::Int,
            Rule::iarray_t => Self::Array,
            Rule::ref_t => Self::Ref,
            Rule::map_t => {
                let mut inner = pair.into_inner();
                let k = Box::new(Type::from_pest(inner.next().unwrap()));
                let v = Box::new(Type::from_pest(inner.next().unwrap()));
                Self::Map(k, v)
            }
            Rule::seq_t => Self::Seq(Box::new(Type::from_pest(pair.into_inner().next().unwrap()))),
            Rule::set_t => Self::Set(Box::new(Type::from_pest(pair.into_inner().next().unwrap()))),
            Rule::shape_t => {
                let shape = Shape::parse(pair.as_str()).unwrap();
                match shape {
                    Shape::Simple => Self::Int,
                    Shape::Nested(inner) => Type::Struct(inner),
                }
            }
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for Decl {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::decl => {
                let mut inner = pair.into_inner();
                let ident = inner.next().unwrap();
                let name = match ident.as_rule() {
                    Rule::ident => ident.as_str().to_owned(),
                    _ => unreachable!(),
                };
                let typ = Type::from_pest(inner.next().unwrap());
                Self { name, typ }
            }
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for Quantifier {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::forall => Self::Forall,
            Rule::exists => Self::Exists,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for Quantified {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let mut pairs = pair.into_inner();
        let decl_amount = pairs.len() - 3;
        assert!(decl_amount > 0);
        let quantifier = Quantifier::from_pest(pairs.next().unwrap());
        let decls = pairs
            .clone()
            .take(decl_amount)
            .map(|d| Decl::from_pest(d))
            .collect();
        let mut pairs = pairs.skip(decl_amount);
        let triggers = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|e| parse_expr(Pairs::single(e)))
            .collect::<Vec<_>>();
        let body = Box::new(parse_expr(Pairs::single(pairs.next().unwrap())));

        Quantified {
            quantifier,
            triggers,
            decls,
            body,
        }
    }
}

impl FromPestPair for Permission {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::perm_write => Self::Write,
            Rule::perm_read => Self::Read,
            Rule::perm_wildcard => Self::Wildcard,
            Rule::perm_frac => {
                let mut inner = pair.into_inner();
                Self::Fractional(
                    inner.next().unwrap().as_str().parse().unwrap(),
                    inner.next().unwrap().as_str().parse().unwrap(),
                )
            }
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for AccessPredicate {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let field = Box::new(parse_expr(Pairs::single(inner.next().unwrap())));
        let perm = inner
            .next()
            .map(Permission::from_pest)
            .unwrap_or(Permission::Write);
        Self { field, perm }
    }
}

impl FromPestPair for SliceType {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::slice_inc => Self::Inclusive,
            Rule::slice_exc => Self::Exclusive,
            _ => unreachable!(),
        }
    }
}

impl FromPestPair for AccessSlice {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let mut inner = pair.into_inner();
        let field = Box::new(parse_expr(Pairs::single(inner.next().unwrap())));
        let lower = Box::new(parse_expr(Pairs::single(inner.next().unwrap())));
        let typ = SliceType::from_pest(inner.next().unwrap());
        let upper = Box::new(parse_expr(Pairs::single(inner.next().unwrap())));
        let perm = inner
            .next()
            .map(Permission::from_pest)
            .unwrap_or(Permission::Write);
        Self {
            field,
            typ,
            lower,
            upper,
            perm,
        }
    }
}

impl FromPestPair for FunctionCall {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let mut pairs = pair.into_inner();
        let fname = pairs.next().unwrap().as_str().to_owned();
        let args = pairs.map(|a| parse_expr(Pairs::single(a))).collect();
        Self { fname, args }
    }
}

impl FromPestPair for UnfoldingIn {
    fn from_pest(pair: Pair<'_, Rule>) -> Self {
        let mut pairs = pair.into_inner();
        let pred = FunctionCall::from_pest(pairs.next().unwrap());
        let expr = parse_expr(Pairs::single(pairs.next().unwrap()));
        Self {
            pred: Box::new(Expr::FunctionCall(pred)),
            expr: Box::new(expr),
        }
    }
}
