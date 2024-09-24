use super::parser::parse_annot;

#[test]
fn t() {
    let a = parse_annot("requires 1 == 0");
    println!("A {:?}", a);
}

#[test]
fn quantifier() {
    let a = parse_annot("ensures forall i: Int :: i >= 0");
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
