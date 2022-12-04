use rand::prelude::*;

use clap::{Args, Parser, Subcommand};
#[derive(Parser)]
#[command(author, version, about)]
struct ArgParser {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Execute(ExecuteArgs),
    Generate(GenerateArgs),
}

#[derive(Args)]
struct ExecuteArgs {
    #[arg(value_parser = parse_u64)]
    code: u64,
}

#[derive(Args)]
struct GenerateArgs {
    #[arg(short, long)]
    random: bool,
    #[arg(short, long)]
    epsilon: Option<f64>,
    #[arg(short, long)]
    stop: Option<f64>,
    #[arg(short, long, value_parser = parse_goal)]
    goal: Option<f64>,
    #[arg(short, long)]
    max: Option<u64>,
}

fn main() {
    let matches = ArgParser::parse();

    match &matches.command {
        Commands::Execute(exe_args) => {
            debug_code(exe_args.code);
        }
        Commands::Generate(gen_args) => {
            let epsilon = gen_args.epsilon.unwrap_or(f64::INFINITY);
            let stop = gen_args.stop.unwrap_or(-1.0);
            let goal = gen_args.goal.unwrap_or(std::f64::consts::E);
            let max = gen_args.max.unwrap_or(std::u64::MAX);
            let mut count: u64 = 0;
            if gen_args.random {
                loop {
                    count += 1;
                    let code: u64 = random();
                    let result = f64::from_be_bytes(code.to_be_bytes());
                    let abs_diff = (result - goal).abs();
                    if abs_diff < epsilon {
                        println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                    }
                    if abs_diff < stop || count == max {
                        println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                        break;
                    }
                }
            } else {
                loop {
                    count += 1;
                    let code = random();
                    let result = execute_code(code);
                    let abs_diff = (result - goal).abs();
                    if abs_diff < epsilon {
                        println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                    }
                    if abs_diff < stop || count == max {
                        println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                        break;
                    }
                }
            }
        }
    }
}

// put > 256 * 3 into r1
// 01010111
// square it
// 11000101
// now set r2 to 1
// 10001010
// copy to r0
// 01100010
// r2 = r2/r1
// 11011001
// r0 = r0 + r2
// 10100010
// r0 = r0 ^ r1
// 11100001

// all together now
// n = (256*3)^2
// 01010111_11000101_11000101_10001010_01100010_11011001_10100010_11100001
// n = (256*3)^2^2
// 01010111_11000101_11000101_11000101_10001010_01100010_11011001_10100010

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

fn instr(byte: u8) -> u8 {
    (byte & 0xe0) >> 5
}

fn op(byte: u8) -> bool {
    byte & 0b0001_0000 != 0
}

fn l_reg(byte: u8) -> usize {
    ((byte & 0b0000_1100) >> 2) as usize
}

fn r_reg(byte: u8) -> usize {
    (byte & 0b0000_0011) as usize
}

fn execute_byte(byte: u8, mut regs: [f64; 4], prev_byte: u8) -> ([f64; 4], Option<usize>) {
    match instr(byte) {
        0b000 => { /* noop */ }
        0b001 => {
            /* return */
            return (regs, Some(r_reg(byte)));
        }
        0b010 => {
            /* load */
            let v = byte & 0b11;
            let dest = &mut regs[l_reg(byte)];
            if op(byte) {
                //regs[l] = (r as u32) << 8 +
                *dest = (((v as u32) << 8) + (prev_byte as u32)) as f64;
            } else {
                *dest = v as f64;
            }
        }
        0b011 => {
            /* move | copy */
            regs[l_reg(byte)] = regs[r_reg(byte)];
            if op(byte) {
                regs[r_reg(byte)] = 0f64;
            }
        }
        0b100 => {
            /* inc | dec */
            let l_reg = l_reg(byte);
            let r_reg = r_reg(byte);
            if op(byte) {
                regs[l_reg] = regs[r_reg] - 1.0;
            } else {
                regs[l_reg] = regs[r_reg] + 1.0;
            }
        }
        0b101 => {
            /* add | sub */
            let l_reg = l_reg(byte);
            let r_reg = r_reg(byte);
            if op(byte) {
                regs[l_reg] -= regs[r_reg];
            } else {
                regs[l_reg] += regs[r_reg];
            }
        }
        0b110 => {
            /* mul | div */
            let l_reg = l_reg(byte);
            let r_reg = r_reg(byte);
            if op(byte) {
                regs[l_reg] /= regs[r_reg];
            } else {
                regs[l_reg] *= regs[r_reg];
            }
        }
        0b111 => {
            let l_reg = l_reg(byte);
            let r_reg = r_reg(byte);
            if op(byte) {
                regs[l_reg] = regs[l_reg].powf(-regs[r_reg]);
            } else {
                regs[l_reg] = regs[l_reg].powf(regs[r_reg]);
            }
        }
        _ => unreachable!(),
    }
    (regs, None)
}

fn execute_code(code: u64) -> f64 {
    let mut regs = [0.0f64; 4];
    let bytes = code.to_be_bytes();

    let mut ret_reg = None;
    // weird circular trick here
    let mut prev_byte = bytes[7];

    for byte in bytes {
        (regs, ret_reg) = execute_byte(byte, regs, prev_byte);
        if ret_reg.is_some() {
            break;
        }
        prev_byte = byte;
    }

    regs[ret_reg.unwrap_or(0)]
}

fn decode_byte(byte: u8, prev_byte: u8) -> String {
    let instr = instr(byte);
    let op = op(byte);
    let l_reg = l_reg(byte);
    let r_reg = r_reg(byte);
    match instr {
        0b000 => format!("NOP"),
        0b001 => format!("RET r{}", r_reg),
        0b010 => {
            if op {
                format!(
                    "LOD r{} <- {}",
                    l_reg,
                    (((r_reg as u32) << 8) + (prev_byte as u32)) as f64
                )
            } else {
                format!("LOD r{} <- {}", l_reg, r_reg)
            }
        }
        0b011 => {
            if op {
                format!("MOV r{} <- r{}", l_reg, r_reg)
            } else {
                format!("COP r{} <- r{}", l_reg, r_reg)
            }
        }
        0b100 => {
            if op {
                format!("DEC r{} <- r{} - 1", l_reg, r_reg)
            } else {
                format!("INC r{} <- r{} + 1", l_reg, r_reg)
            }
        }
        0b101 => {
            if op {
                format!("SUB r{0} <- r{0} - r{1}", l_reg, r_reg)
            } else {
                format!("ADD r{0} <- r{0} + r{1}", l_reg, r_reg)
            }
        }
        0b110 => {
            if op {
                format!("DIV r{0} <- r{0} / r{1}", l_reg, r_reg)
            } else {
                format!("MUL r{0} <- r{0} * r{1}", l_reg, r_reg)
            }
        }
        0b111 => {
            if op {
                format!("NXP r{0} <- r{0} ^-r{1}", l_reg, r_reg)
            } else {
                format!("EXP r{0} <- r{0} ^ r{1}", l_reg, r_reg)
            }
        }
        _ => String::new(),
    }
}

fn debug_code(code: u64) -> f64 {
    eprintln!("0b{:0>64b}", code);
    let mut regs = [0.0f64; 4];
    let bytes = code.to_be_bytes();

    // weird circular trick here
    let mut prev_byte = bytes[7];

    for byte in bytes {
        let ret_reg;
        (regs, ret_reg) = execute_byte(byte, regs, prev_byte);
        eprintln!(
            "0x{:0>2x} {: <17} {:?} ",
            byte,
            decode_byte(byte, prev_byte),
            regs
        );
        if let Some(ret_addr) = ret_reg {
            eprintln!("reg{} [{}]", ret_addr, regs[ret_addr]);
            return regs[ret_addr];
        }
        prev_byte = byte;
    }

    // if nothing was returned, look at register 0
    eprintln!("reg0 [{}]", regs[0]);
    regs[0]
}

#[cfg(test)]
mod test_randcode {
    use super::*;

    fn assert_code_returns(code: u64, expected: f64) {
        assert_eq!(execute_code(code), expected);
    }

    #[test]
    fn test_load() {
        assert_code_returns(0, 0.0);
        // loading a small value
        assert_code_returns(0b0100_0001, 1.0);
        assert_code_returns(0b0100_0011, 3.0);
        // loading a large value using previous byte
        assert_code_returns(0b0000_0111_0101_0011, 7.0 + 256.0 * 3.0);
    }

    #[test]
    fn test_return() {
        // loading a value into reg0 but then return reg1 instead
        assert_code_returns(0b0100_0011_0010_0001, 0.0);
        // now return from reg1
        assert_code_returns(0b0100_0111_0010_0001, 3.0);
        // try reg3
        assert_code_returns(0b0100_1111_0010_0011, 3.0);
    }

    #[test]
    fn test_mov() {
        // load 3 into reg3, the move to reg0
        assert_code_returns(0b0100_1111_0110_0011, 3.0);
        // test that 3 is still in reg3
        assert_code_returns(0b0100_1111_0110_0011_0010_0011, 3.0);
        // ... but if we mov, then 3 should be cleared
        assert_code_returns(0b0100_1111_0111_0011_0010_0011, 0.0);
    }

    #[test]
    fn test_inc_dec() {
        // increment reg0
        assert_code_returns(0b1000_0000, 1.0);
        // decrement reg0
        assert_code_returns(0b1001_0000, -1.0);
        // load into reg3, inc it, put in reg0
        assert_code_returns(0b0000_0111_0101_1111_1000_0011, 7.0 + 256.0 * 3.0 + 1.0);
        // confirm no effects on reg3
        // load into reg3, inc it, put in reg0
        assert_code_returns(0b0000_0111_0101_1111_1000_0011_0010_0111, 7.0 + 256.0 * 3.0);
    }

    #[test]
    fn test_add_sub() {
        // set r0 and r2 to 3, then add them
        assert_code_returns(0b0100_0011_0100_1011_1010_0010, 6.0);
        // set r0 to 1 and r2 to 3, then sub them
        assert_code_returns(0b0100_0001_0100_1011_1011_0010, -2.0);
    }

    #[test]
    fn test_mul_div() {
        // set r0 and r2 to 3, then multiply them
        assert_code_returns(0b0100_0011_0100_1011_1100_0010, 9.0);
        // set r0 to 1 and r2 to 3, then divide them
        assert_code_returns(0b0100_0001_0100_1011_1101_0010, 1.0 / 3.0);
    }

    #[test]
    fn test_exp() {
        // set r0 and r2 to 3, then exponentiate them
        assert_code_returns(0b0100_0011_0100_1011_1110_0010, 27.0);
        // set r0 to 8 and r2 to 3, then negative exponentiate them
        assert_code_returns(0b0100_0010_0100_1011_1111_0010, 0.125);
    }
}
/*

Register Operations:
| _ _ _ | _ | | _ _| | _ _ |
 instr.  op    dest  right


Arithmetic Operations
| _ _ _ | _ | | _ _ | _ _ |
  inst.   op   left   right



0x0 NOP: noop
0x1 RET: return right register
0x2 LDR: load register
    - if op is 0, then load value into dest (as a i2)
    - if op is 1, then load right | prev byte into dest as two's complement
0x3 MOV: copy register
    - if op is 0, then copy right to left
    - if op is 1, then move right to left (clearing right to 0)
0x4 INC (DEC): increment (or decrement) right, store result in left
0x5 ADD (SUB): addition (subtract) left by right, store result in left
0x6 MUL (DIV): multiply (divide) left by right, store result in left
0x7 EXP (NXP): raise left to (negative) right power, store result in left
*/
