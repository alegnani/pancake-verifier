use super::*;

#[test]
fn dec_call() {
    let pnk = "
        fun main() {
            return foo();
        }

        fun foo() {
            return 1;
        }
    ";
}
