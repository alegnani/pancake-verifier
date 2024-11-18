use std::env;

use app::App;

use super::*;

fn verify_file(path: &str) -> anyhow::Result<()> {
    let mut app = App::default();
    app.code = std::fs::read_to_string(path)?;
    app.verify = true;

    Ok(())
}

fn verify_file_model(path: &str, model: &str) -> anyhow::Result<()> {
    let mut app = App::default();
    app.code = std::fs::read_to_string(path).unwrap();
    app.model = Some(std::fs::read_to_string(model)?);
    app.verify = true;

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
