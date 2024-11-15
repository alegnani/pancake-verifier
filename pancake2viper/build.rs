use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = PathBuf::from(out_dir).join("generated_tests.rs");
    let mut test_file = fs::File::create(dest_path).unwrap();

    for entry in fs::read_dir("./tests/pass").expect("Test directory not found") {
        let entry = entry.unwrap();
        let file = entry.path().canonicalize().unwrap();
        let file_stem = file.file_stem().unwrap().to_str().unwrap();
        let file_ext = file.extension().unwrap_or_default().to_str().unwrap();
        let path = file.display();

        if file.is_file() && file_ext == "pnk" {
            writeln!(
                test_file,
                r#"
                #[test]
                fn test_{file_stem}() {{
                    verify_file("{path}").unwrap();
                }}
                "#,
            )
            .unwrap();
        }
    }
    // TODO: add shared memory model tests
}
