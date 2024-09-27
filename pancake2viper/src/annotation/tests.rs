use crate::ir::*;

use super::parser::parse_annot;

#[test]
fn t() {
    let a = parse_annot("requires 1 == 0");
    println!("A {:?}", a);
}

#[test]
fn quantifier() {
    let a = parse_annot("ensures forall i: Int :: i == 0");
    println!("{:?}", a);
}

#[test]
fn trigger() {
    let a = parse_annot("ensures forall i: Int :: {slot(i)} slot(i) <= 0");
    println!("{:?}", a);
}

#[test]
fn fapp1() {
    let a = parse_annot("ensures f()");
    println!("{:?}", a);
}

#[test]
fn fapp2() {
    let a = parse_annot("ensures f(1, g())");
    println!("{:?}", a);
}

#[test]
fn parenthesis1() {
    let a = parse_annot("invariant (accu == (i - 1) * i / 2)");
    println!("{:?}", a);
}

#[test]
fn precedence() {
    let a = parse_annot("invariant 1 + 1 * 2)");
    println!("{:?}", a);
    match a.expr {
        Expr::BinOp(BinOp {
            optype: BinOpType::Add,
            left,
            right,
        }) => {
            match *left {
                Expr::Const(1) => (),
                _ => panic!(),
            };
            match *right {
                Expr::BinOp(_) => (),
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}
