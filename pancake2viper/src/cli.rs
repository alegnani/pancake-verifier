use std::env;

use clap::{Args, Parser, Subcommand, ValueEnum};
use clap_stdin::FileOrStdin;

use crate::utils::EncodeOptions;

#[derive(Subcommand, Debug, Clone)]
pub enum ClapCommand {
    /// Transpiles the given Pancake file to Viper
    Transpile(ClapTranspile),
    /// Verifies the given Pancake file
    Verify(ClapVerify),
    /// Transpiles and verifies the given Pancake file
    TranspileVerify(ClapTranspileVerify),
    /// Generates the boilerplate Viper file for the shared memory model
    Generate(ClapGenerate),
}

#[derive(Debug, Clone)]
pub enum Command {
    Transpile(Transpile),
    Verify(Verify),
    TranspileVerify(TranspileVerify),
    Generate(Generate),
}

impl From<ClapCommand> for Command {
    fn from(value: ClapCommand) -> Self {
        match value {
            ClapCommand::Transpile(t) => Self::Transpile(t.into()),
            ClapCommand::Verify(v) => Self::Verify(v.into()),
            ClapCommand::TranspileVerify(t) => Self::TranspileVerify(t.into()),
            ClapCommand::Generate(g) => Self::Generate(g.into()),
        }
    }
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
    }

    pub fn get_output_path(&self) -> Option<String> {
        match self {
            Self::Verify(_) => None,
            Self::Transpile(Transpile { output_path, .. })
            | Self::TranspileVerify(TranspileVerify { output_path, .. })
            | Self::Generate(Generate { output_path, .. }) => Some(output_path.clone()),
        }
    }

    pub fn is_verify(&self) -> bool {
        matches!(self, Self::TranspileVerify(_) | Self::Verify(_))
    }
    pub fn is_generate(&self) -> bool {
        matches!(self, Self::Generate(_))
    }
}

#[derive(Debug, Clone, Args)]
pub struct ClapTranspile {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
    /// Destination path of transpiled file
    output_path: String,
}

#[derive(Debug, Clone)]
pub struct Transpile {
    pub input: String,
    pub output_path: String,
}

impl From<ClapTranspile> for Transpile {
    fn from(value: ClapTranspile) -> Self {
        Self {
            input: value.input.contents().unwrap(),
            output_path: value.output_path,
        }
    }
}

#[derive(Debug, Clone, Args)]
pub struct ClapVerify {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
}

#[derive(Debug, Clone)]
pub struct Verify {
    pub input: String,
}

impl From<ClapVerify> for Verify {
    fn from(value: ClapVerify) -> Self {
        Self {
            input: value.input.contents().unwrap(),
        }
    }
}

#[derive(Debug, Clone, Args)]
pub struct ClapTranspileVerify {
    /// Path of Pancake file to be transpiled
    input: FileOrStdin<String>,
    /// Destination path of transpiled file
    output_path: String,
}

#[derive(Debug, Clone)]
pub struct TranspileVerify {
    pub input: String,
    pub output_path: String,
}

impl From<ClapTranspileVerify> for TranspileVerify {
    fn from(value: ClapTranspileVerify) -> Self {
        Self {
            input: value.input.contents().unwrap(),
            output_path: value.output_path,
        }
    }
}

#[derive(Debug, Clone, Args)]
pub struct ClapGenerate {
    /// Path of Pancake file with shared memory prototypes
    input: FileOrStdin<String>,
    /// Destination path of generated file
    output_path: String,
}

#[derive(Debug, Clone)]
pub struct Generate {
    pub input: String,
    pub output_path: String,
}

impl From<ClapGenerate> for Generate {
    fn from(value: ClapGenerate) -> Self {
        Self {
            input: value.input.contents().unwrap(),
            output_path: value.output_path,
        }
    }
}

#[derive(Debug, Parser, Clone)]
#[command(version, about, rename_all = "kebab-case")]
pub struct ClapCliOptions {
    #[command(subcommand)]
    pub cmd: ClapCommand,
    //
    #[arg(global = true, short, long, default_value_t=WordSize::Bits64, value_enum, help = "Specify the word size in bits")]
    pub word_size: WordSize,

    #[arg(
        global = true,
        long,
        help = "Skip checking that variables don't under- or overflow their size"
    )]
    pub disable_overflow_check: bool,

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

    #[arg(
        global = true,
        value_delimiter = ' ',
        short,
        long,
        help = "Verify only the following function(s) (multiple names separate by spaces)"
    )]
    pub only: Option<Vec<String>>,

    #[arg(global = true, long, help = "Verifies each function separately")]
    pub incremental: bool,

    #[arg(
        global = true,
        long,
        help = "Trust and skip verification of model methods"
    )]
    pub trust_model: bool,

    #[arg(global = true, long, short = 'I', help = "Include Viper files")]
    pub include: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct CliOptions {
    pub cmd: Command,
    pub word_size: WordSize,
    pub disable_overflow_checks: bool,
    pub bounded_arithmetic: bool,
    pub disable_assert_alignment: bool,
    pub heap_size: u64,
    pub cake_path: String,
    pub viper_path: String,
    pub z3_exe: String,
    pub debug_comments: bool,
    pub disable_prelude: bool,
    pub disable_return_post: bool,
    pub model: Option<String>,
    pub allow_undefined_shared: bool,
    pub ignore_warnings: bool,
    pub only: Option<Vec<String>>,
    pub incremental: bool,
    pub trust_model: bool,
    pub include: Vec<String>,
}

impl From<ClapCliOptions> for CliOptions {
    fn from(value: ClapCliOptions) -> Self {
        Self {
            cmd: value.cmd.into(),
            word_size: value.word_size,
            disable_overflow_checks: value.disable_overflow_check,
            bounded_arithmetic: value.bounded_arithmetic,
            disable_assert_alignment: value.disable_assert_alignment,
            heap_size: value.heap_size,
            cake_path: value.cake_path,
            viper_path: value.viper_path,
            z3_exe: value.z3_exe,
            debug_comments: value.debug_comments,
            disable_prelude: value.disable_prelude,
            disable_return_post: value.disable_return_post,
            model: value.model.map(|f| f.contents().unwrap()),
            allow_undefined_shared: value.allow_undefined_shared,
            ignore_warnings: value.ignore_warnings,
            only: value.only,
            incremental: value.incremental,
            trust_model: value.trust_model,
            include: value.include.unwrap_or_default(),
        }
    }
}

impl Default for CliOptions {
    fn default() -> Self {
        Self {
            cmd: Command::Verify(Verify { input: "".into() }),
            word_size: WordSize::Bits64,
            disable_overflow_checks: false,
            bounded_arithmetic: false,
            disable_assert_alignment: false,
            heap_size: 16384,
            cake_path: get_cake_path(),
            viper_path: get_viper_path(),
            z3_exe: get_z3_path(),
            debug_comments: false,
            disable_prelude: false,
            disable_return_post: true,
            model: None,
            allow_undefined_shared: false,
            ignore_warnings: false,
            only: None,
            incremental: false,
            trust_model: false,
            include: vec![],
        }
    }
}

impl From<CliOptions> for EncodeOptions {
    fn from(value: CliOptions) -> Self {
        Self {
            assert_aligned_accesses: !value.disable_assert_alignment,
            word_size: value.word_size.into(),
            heap_size: value.heap_size,
            check_overflows: !value.disable_overflow_checks,
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
