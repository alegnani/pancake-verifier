use clap::Parser;
use pancake2viper::{app::App, cli::CliOptions};

fn main() -> anyhow::Result<()> {
    let options = CliOptions::parse();
    let app = App::new(options, true);
    app.run()
}
