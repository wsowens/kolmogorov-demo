use clap::Parser;
use rand::prelude::*;

mod machine16bit;
mod parser;
use crate::parser::{Cli, Command};

fn main() {
    let input = Cli::parse();

    match &input.command {
        Command::Execute(args) => {
            let params = machine16bit::MachineParams {
                max_cycles: args.max_cycles,
                verbose: args.verbose,
            };
            let result = machine16bit::execute_code_params(&args.code[..], &params);
            if args.verbose {
                println!("{:?}", result)
            } else {
                println!("{:?}", result.value())
            }
        }
        Command::Decompile(args) => {
            println!("{}", machine16bit::format_code(&args.code[..]));
        }
        Command::Generate(args) => {
            if args.no_code {
                generate_floats(args);
            } else {
                generate_code(args);
            }
        }
    }
}

fn generate_floats(params: &parser::GenerateArgs) {
    let mut count = 0;
    loop {
        if params.max_tests.map_or(false, |m| m == count) {
            break;
        }

        let result: f64 = random();

        match params.goal {
            Some(goal) => {
                let abs_diff = (result - goal).abs();

                if abs_diff < params.stop {
                    println!("{}\t{:.16}\t{:.16}", count, result, abs_diff);
                    break;
                }
                if abs_diff < params.epsilon {
                    println!("{}\t{:.16}\t{:.16}", count, result, abs_diff);
                }
            }
            None => {
                println!("{}\t{:.16}", count, result);
            }
        }
        count += 1;
    }
}

fn generate_code(params: &parser::GenerateArgs) {
    let mut count = 0;

    let mut machine_params = machine16bit::MachineParams::default();
    machine_params.max_cycles = params.max_cycles;

    loop {
        if params.max_tests.map_or(false, |m| m == count) {
            break;
        }

        let code: u128 = random();

        let return_result =
            machine16bit::execute_code_params(&code.to_be_bytes()[..], &machine_params);
        let result = return_result.value();

        match params.goal {
            Some(goal) => {
                let abs_diff = (result - goal).abs();

                if abs_diff < params.epsilon {
                    println!(
                        "{}\t0x{:0>16x}\t{:?}\t{:.16}\t{:.16}",
                        count, code, return_result, result, abs_diff
                    );
                }
                if abs_diff < params.stop {
                    println!(
                        "{}\t0x{:0>16x}\t{:?}\t{:.16}\t{:.16}",
                        count, code, return_result, result, abs_diff
                    );
                    break;
                }
            }
            None => {
                println!(
                    "{}\t0x{:0>16x}\t{:?}\t{:.16}",
                    count, code, return_result, result
                );
            }
        }
        count += 1;
    }
}
