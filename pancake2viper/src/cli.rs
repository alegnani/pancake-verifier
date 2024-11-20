use std::env;

use clap::{Args, Parser, Subcommand, ValueEnum};
use clap_stdin::FileOrStdin;

use crate::utils::EncodeOptions;

#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    /// Transpiles the given Pancake file to Viper
    Transpile(Transpile),
    /// Verifies the given Pancake file
    Verify(Verify),
    /// Transpiles and verifies the given Pancake file
    TranspileVerify(TranspileVerify),
    /// Generates the boilerplate Viper file for the shared memory model
    Generate(Generate),
}

impl Command {
    pub fn get_input(&self) -> String {
        match self {
            Self::Verify(v) => &v.input,
            Self::Transpile(v) => &v.input,
            Self::TranspileVerify(v) => &v.input,
            Self::Generate(v) => &v.input,
        }
        .clone()
        .contents()
        .unwrap()
    }

    pub fn get_output_path(&self) -> Option<String> {
        match self {
            Self::Verify(_) => None,
            Self::Transpile(Transpile { output_path, .. })
            | Self::TranspileVerify(TranspileVerify { output_path, .. })
            | Self::Generate(Generate { output_path, .. }) => Some(output_path.clone()),
        }
    }

    pub fn get_verify(&self) -> bool {
        matches!(self, Self::TranspileVerify(_) | Self::Verify(_))
    }
}

#[derive(Debug, Clone, Args)]
pub struct Transpile {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
    /// Destination path of transpiled file
    output_path: String,
}

#[derive(Debug, Clone, Args)]
pub struct Verify {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
}

#[derive(Debug, Clone, Args)]
pub struct TranspileVerify {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
    /// Destination path of transpiled file
    output_path: String,
}

#[derive(Debug, Clone, Args)]
pub struct Generate {
    /// Path of Pancake file with shared memory prototypes
    input: FileOrStdin<String>,
    /// Destination path of generated file
    output_path: String,
}

#[derive(Debug, Parser, Clone)]
#[command(version, about, rename_all = "kebab-case")]
pub struct CliOptions {
    #[command(subcommand)]
    pub cmd: Command,
    //
    #[arg(global = true, short, long, default_value_t=WordSize::Bits64, value_enum, help = "Specify the word size in bits")]
    pub word_size: WordSize,

    #[arg(
        global = true,
        long,
        help = "Unroll expressions into Three-address code (experimental)"
    )]
    pub tac: bool,

    #[arg(
        global = true,
        long,
        help = "Check that variables don't under- or overflow their size"
    )]
    pub check_overflows: bool,

    #[arg(
        global = true,
        long,
        help = "Model arithmetic operations as bounded (implicit under- or overflows)"
    )]
    pub bounded_arithmetic: bool,

    #[arg(
        global = true,
        long,
        help = "Removes assertions for alignment of memory operations"
    )]
    pub disable_assert_alignment: bool,

    #[arg(
        global = true,
        long,
        default_value_t = 16384,
        help = "Maximum valid offset from @base in bytes"
    )]
    pub heap_size: u64,

    #[arg(
        global = true,
        long = "cake",
        default_value_t = get_cake_path(),
        help = "Path to the cake compiler, can be set via $CAKE_ML"
    )]
    pub cake_path: String,

    #[arg(
        global = true,
        long = "viper",
        default_value_t = get_viper_path(),
        help = "Path to Viper installation, can be set via $VIPER_HOME"
    )]
    pub viper_path: String,

    #[arg(
        global = true,
        long = "z3",
        default_value_t = get_z3_path(),
        help = "Path to z3, can be set via $Z3_EXE"
    )]
    pub z3_exe: String,

    #[arg(global = true, long, help = "Add debug comments to transpiled Viper")]
    pub debug_comments: bool,

    #[arg(
        global = true,
        long,
        help = "Does not include the transpilation prelude (helper functions)"
    )]
    pub disable_prelude: bool,

    #[arg(
        global = true,
        long,
        help = "Does not add the postconditions for composite shape returns"
    )]
    pub disable_return_post: bool,

    #[arg(global = true, short, long, help = "Shared memory model file")]
    pub model: Option<FileOrStdin<String>>,

    #[arg(
        global = true,
        long,
        help = "Allows accessing shared memory that has not been previously bound to method in the shared memory model"
    )]
    pub allow_undefined_shared: bool,

    #[arg(global = true, long, help = "Ignore warnings")]
    pub ignore_warnings: bool,
}

impl From<CliOptions> for EncodeOptions {
    fn from(value: CliOptions) -> Self {
        Self {
            expr_unrolling: value.tac,
            assert_aligned_accesses: !value.disable_assert_alignment,
            word_size: value.word_size.into(),
            heap_size: value.heap_size,
            check_overflows: value.check_overflows,
            bounded_arithmetic: value.bounded_arithmetic,
            debug_comments: value.debug_comments,
            include_prelude: !value.disable_prelude,
            return_post: !value.disable_return_post,
            allow_undefined_shared: value.allow_undefined_shared,
            ignore_warnings: value.ignore_warnings,
        }
    }
}

pub fn get_viper_path() -> String {
    env::var("VIPER_HOME").expect(
        "Path to Viper installation is not provided, try setting it via $VIPER_HOME or --viper",
    )
}

pub fn get_cake_path() -> String {
    env::var("CAKE_ML")
        .expect("Path to CakeML compiler is not provided, try setting it via $CAKE_ML or --cake")
}

pub fn get_z3_path() -> String {
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
