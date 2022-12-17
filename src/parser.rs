use clap::{Args, Parser, Subcommand};
use rand::prelude::*;

#[derive(Parser)]
#[command(author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    /// Execute a code snippet
    Execute(ExecuteArgs),
    /// Display a decompiled version of a code snippet.
    Decompile(DecompileArgs),
    /// Generate and run random code snippets.
    /// Optionally look for snippets that produce outputs near some value.
    Generate(GenerateArgs),
}

#[derive(Args)]
pub struct ExecuteArgs {
    /// Code snippet to test.
    /// Can be provided as decimal, binary ('0b') or hexadecimal ('0x').
    #[arg(value_parser = parse_bytes)]
    pub code: std::vec::Vec<u8>,

    /// Maximum number of cycles allowed per code snippet. [default: None]
    #[arg(short, long)]
    pub max_cycles: Option<usize>,

    /// Detect (and abort) infinitely looping code snippets.
    #[arg(long)]
    pub detect_loops: bool,

    /// If enabled, print a full step-by-step trace of each program.
    #[arg(short, long)]
    pub verbose: bool,
}

#[derive(Args)]
pub struct DecompileArgs {
    /// Code snippet to display.
    /// Can be provided as decimal, binary ('0b') or hexadecimal ('0x').
    #[arg(value_parser = parse_bytes)]
    pub code: std::vec::Vec<u8>,
}

#[derive(Args)]
pub struct GenerateArgs {
    /// Maximum number of cycles allowed per code snippet.
    #[arg(short, long)]
    pub max_cycles: Option<usize>,

    /// Detect (and abort) infinitely looping code snippets.
    #[arg(long)]
    pub detect_loops: bool,

    /// If enabled, search for snippets close to some goal.
    #[arg(short, long, value_parser = parse_goal)]
    pub goal: Option<f64>,

    /// Maximum number of snippets to test. [default: None]
    #[arg(long)]
    pub max_tests: Option<usize>,

    /// Only display snippets within some range epsilon of the goal.
    #[arg(short, long, default_value_t=f64::INFINITY)]
    pub epsilon: f64,

    /// Stop once this many bits are accurate.
    #[arg(short, long, default_value_t=-1.0)]
    pub stop: f64,

    /// If enabled, generate floats directly instead of interpreting code.
    #[arg(long)]
    pub no_code: bool,
}

fn parse_goal(s: &str) -> Result<f64, std::num::ParseFloatError> {
    if s.starts_with("pi") {
        Ok(std::f64::consts::PI)
    } else if s.starts_with("e") {
        Ok(std::f64::consts::E)
    } else if s.starts_with("rand") {
        // a random value near e
        Ok(2.0 + random::<f64>())
    } else {
        s.parse()
    }
}

fn parse_bytes(s: &str) -> Result<Vec<u8>, std::num::ParseIntError> {
    let s = s.replace("_", "");
    let mut bytes: Vec<u8> = Vec::with_capacity(s.len() / 2);
    let mut radix = 10;
    let mut i = 0;
    if s.starts_with("0x") {
        radix = 16;
        i = 2;
    } else if s.starts_with("0o") {
        radix = 8;
        i = 2;
    } else if s.starts_with("0b") {
        radix = 2;
        i = 2;
    }
    while i < s.len() {
        bytes.push(u8::from_str_radix(&s[i..i + 2], radix)?);
        i += 2;
    }
    Ok(bytes)
}
