use crate::{annotation::parser::FromPestPair, ir::*};

use super::{parse_function, parser::parse_annot};

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

#[test]
fn functions() {
    let f = "/@ function sum(heap: IArray, base: Int, len: Int): Int 
    requires base >= 0 && len >= 0 
    requires base + len <= alen(heap)
    requires forall i: Int :: base <= i && i < base + len ==> acc(heap[i], read)
    { 1 }
    @/";
    println!("{:?}", parse_function(f));
}

#[test]
fn ternary() {
    let t = "assert x == y ? f(x) : f(h) + 1";
    println!("{:?}", parse_annot(t));
}

#[test]
fn seq_type() {
    let f = "/@ function sum(arg: Seq[Set[Int]]): Int 
    { 1 }
    @/";
    println!("{:?}", parse_function(f));
}

#[test]
fn map_type() {
    let f = "/@ function sum(arg: Seq[Map[Int, Bool]]): Int 
    { 1 }
    @/";
    println!("{:?}", parse_function(f));
}
