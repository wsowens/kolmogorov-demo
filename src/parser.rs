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
    /// Parse the instructions in a code snippet
    Debug(ExecuteArgs),
    /// Step through each instruction of a code snippet
    Trace(ExecuteArgs),
    /// Generate and test random snippets of code
    Generate(GenerateArgs),
}

#[derive(Args)]
pub struct ExecuteArgs {
    /// Code snippets to test.
    /// Can be provided as decimal, binary ('0b') or hexadecimal ('0x').
    #[arg(value_parser = parse_u64)]
    pub code: u64,
}

#[derive(Args)]
pub struct GenerateArgs {
    #[arg(short, long, default_value_t=f64::INFINITY)]
    pub epsilon: f64,
    #[arg(short, long, default_value_t=-1.0)]
    pub stop: f64,
    #[arg(short, long, default_value_t=std::f64::consts::E, value_parser = parse_goal)]
    pub goal: f64,
    #[arg(short, long)]
    pub max: Option<u64>,
    /// Generate raw floats but don't interpret them as code.
    #[arg(long)]
    pub no_interpret: bool,
}

fn parse_u64(s: &str) -> Result<u64, std::num::ParseIntError> {
    let s = s.replace("_", "");
    if s.starts_with("0x") {
        u64::from_str_radix(&s[2..], 16)
    } else if s.starts_with("0o") {
        u64::from_str_radix(&s[2..], 8).into()
    } else if s.starts_with("0b") {
        u64::from_str_radix(&s[2..], 2).into()
    } else {
        u64::from_str_radix(&s, 10)
    }
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
