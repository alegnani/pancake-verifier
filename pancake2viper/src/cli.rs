use std::env;

use clap::{Parser, ValueEnum};
use clap_stdin::FileOrStdin;

#[derive(Debug, Parser, Clone)]
#[clap(version, about, rename_all = "kebab-case")]
pub struct CliOptions {
    #[arg(short, long, default_value_t=WordSize::Bits64, value_enum, help = "Specify the word size in bits")]
    pub word_size: WordSize,

    #[arg(
        long,
        help = "Unroll expressions into Three-address code (experimental)"
    )]
    pub tac: bool,

    #[arg(long, help = "Removes assertions for alignment of memory operations")]
    pub disable_assert_alignment: bool,

    #[arg(
        long,
        default_value_t = 16384,
        help = "Maximum valid offset from @base in bytes"
    )]
    pub heap_size: u64,

    #[arg(
        long = "cake",
        default_value_t = get_cake_path(),
        help = "Path to the cake compiler, can be set via $CAKE_ML"
    )]
    pub cake_path: String,

    #[arg(
        long = "viper",
        default_value_t = get_viper_path(),
        help = "Path to Viper installation, can be set via $VIPER_HOME"
    )]
    pub viper_path: String,

    #[arg(
        long = "z3",
        default_value_t = get_z3_path(),
        help = "Path to z3, can be set via $Z3_EXE"
    )]
    pub z3_exe: String,

    pub file: FileOrStdin<String>,

    #[arg(short, long, help = "Writes transpiled Viper to file")]
    pub output_path: Option<String>,

    #[arg(short, long, help = "Writes transpiled Viper to stdout")]
    pub print_transpiled: bool,

    #[arg(short, long, help = "Verify the Pancake code")]
    pub verify: bool,
}

fn get_viper_path() -> String {
    env::var("VIPER_HOME").expect(
        "Path to Viper installation is not provided, try setting it via $VIPER_HOME or --viper",
    )
}

fn get_cake_path() -> String {
    env::var("CAKE_ML")
        .expect("Path to CakeML compiler is not provided, try setting it via $CAKE_ML or --cake")
}

fn get_z3_path() -> String {
    env::var("Z3_EXE").expect("Path to z3 is not provided, try setting it via $Z3_EXE or --z3")
}

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum WordSize {
    #[clap(name = "64")]
    Bits64,
    #[clap(name = "32")]
    Bits32,
}

impl From<WordSize> for u64 {
    fn from(val: WordSize) -> Self {
        match val {
            WordSize::Bits32 => 32,
            WordSize::Bits64 => 64,
        }
    }
}
