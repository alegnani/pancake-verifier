use clap::Parser;
use pancake2viper::{app::App, cli::ClapCliOptions};

fn main() -> anyhow::Result<()> {
    let options = ClapCliOptions::parse();
    let viper = Box::new(viper::Viper::new_with_args(&options.viper_path, vec![]));
    let viper = Box::leak(viper);
    let app = App::new(options.into(), true);
    app.run(viper)
}
