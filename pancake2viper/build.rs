use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

macro_rules! gen_tests {
    ($test_file:ident, $path:literal, $fmt:expr) => {
        for entry in fs::read_dir($path).expect("Test directory not found") {
            let entry = entry.unwrap();
            let file = entry.path().canonicalize().unwrap();
            let file_stem = file.file_stem().unwrap().to_str().unwrap();
            let file_ext = file.extension().unwrap_or_default().to_str().unwrap();
            let path = file.display();

            Command::new("echo")
                .arg(format!("{}", path))
                .spawn()
                .unwrap();
            if file.is_file() && file_ext == "pnk" {
                writeln!($test_file, $fmt, file_stem, path).unwrap();
            }
        }
    };
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = PathBuf::from(out_dir).join("generated_tests.rs");
    let mut test_file = fs::File::create(dest_path).unwrap();

    gen_tests!(
        test_file,
        "./tests/pass",
        r#"
                #[test]
                fn tp_{}() {{
                    verify_file("{}", false).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/fail",
        r#"
                #[test]
                #[should_panic]
                fn tf_{}() {{
                    verify_file("{}", false).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/shared/pass",
        r#"
                #[test]
                fn mp_{}() {{
                    verify_file_model("{}", false,).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/shared/fail",
        r#"
                #[test]
                #[should_panic]
                fn mf_{}() {{
                    verify_file_model("{}", false).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/pass",
        r#"
                #[test]
                fn tp_{}_incremental() {{
                    verify_file("{}", true).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/fail",
        r#"
                #[test]
                #[should_panic]
                fn tf_{}_incremental() {{
                    verify_file("{}", true).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/shared/pass",
        r#"
                #[test]
                fn mp_{}_incremental() {{
                    verify_file_model("{}", true).unwrap();
                }}
                "#
    );
    gen_tests!(
        test_file,
        "./tests/shared/fail",
        r#"
                #[test]
                #[should_panic]
                fn mf_{}_incremental() {{
                    verify_file_model("{}", true).unwrap();
                }}
                "#
    );
}
