use crate::ir::*;

use super::{parse_function, parser::parse_annot};

#[test]
fn t() {
    let a = parse_annot("requires 1 == 0", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn quantifier() {
    let a = parse_annot("ensures forall i: Int :: i == 0", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn trigger() {
    let a = parse_annot("ensures forall i: Int :: {slot(i)} slot(i) <= 0", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn fapp1() {
    let a = parse_annot("ensures f()", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn fapp2() {
    let a = parse_annot("ensures f(1, g())", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn parenthesis1() {
    let a = parse_annot("invariant (accu == (i - 1) * i / 2)", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn precedence() {
    let a = parse_annot("invariant 1 + 1 * 2", true).unwrap();
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

#[test]
fn functions() {
    let f = "/@ function sum(heap: Seq[Ref], base: Int, len: Int): Int 
    requires base >= 0 && len >= 0 
    requires base + len <= alen(heap)
    requires forall i: Int :: base <= i && i < base + len ==> acc(heap[i], read)
    { 1 }
    @/";
    println!("{:?}", parse_function(f).unwrap());
}

#[test]
fn ternary() {
    let t = "assert x == y ? f(x) : f(h) + 1";
    println!("{:?}", parse_annot(t, true).unwrap());
}

#[test]
fn seq_type() {
    let f = "/@ function sum(arg: Seq[Set[Int]]): Int 
    { 1 }
    @/";
    println!("{:?}", parse_function(f).unwrap());
}

#[test]
fn map_type() {
    let f = "/@ function sum(arg: Seq[Map[Int, Bool]]): Int 
    { 1 }
    @/";
    println!("{:?}", parse_function(f).unwrap());
}

#[test]
fn pratt_error() {
    let a = parse_annot(" fold valid_device() ", true).unwrap();
    println!("{:?}", a);
}

#[test]
fn pratt_errort2() {
    let annot = " assert unfolding valid_state() in unfolding seq_pred() in (state.device4[0] == state.device4[1]) ";
    let a = parse_annot(annot, true).unwrap();
    println!("{:?}", a);
}
