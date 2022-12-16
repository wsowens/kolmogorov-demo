use clap::Parser;
use rand::prelude::*;

mod parser;
use crate::parser::{Cli, Command};

fn main() {
    let input = Cli::parse();

    match &input.command {
        // Command::Debug
        Command::Execute(args) => {
            println!("{}", machine8bit::format_code(args.code));
        }
        Command::Debug(args) => {
            println!("{}", machine8bit::format_code(args.code));
        }
        Command::Trace(args) => {
            eprintln!("{}\n", machine8bit::format_code(args.code));
            machine8bit::trace_code(args.code);
        }
        Command::Generate(args) => {
            let mut count: u64 = 0;
            loop {
                count += 1;
                let code: u64 = random();
                eprintln!("{:x}", code);
                let result = if args.no_interpret {
                    f64::from_be_bytes(code.to_be_bytes())
                } else {
                    machine8bit::execute_code(code)
                };
                let abs_diff = (result - args.goal).abs();
                if abs_diff < args.epsilon {
                    println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                }
                if abs_diff < args.stop || (args.max.is_some() && args.max.unwrap() == count) {
                    println!("{}\t0x{:x}\t{:.16}\t{:.16}", count, code, result, abs_diff);
                    break;
                }
            }
        }
    }
}

mod machine8bit {
    use std::cmp::{Eq, Ord, PartialEq, PartialOrd};
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    struct MachineState {
        regs: [f64; 4],
        curr: u8,
        prev: u8,
        addr: usize,
        retval: Option<usize>,
    }

    #[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct HashableMachineState {
        regs: [u64; 4],
        curr: u8,
        prev: u8,
        addr: usize,
        retval: Option<usize>,
    }

    impl MachineState {
        fn init(code: u64) -> ([u8; 8], MachineState) {
            let bytes = code.to_be_bytes();
            (
                bytes,
                MachineState {
                    regs: [0.0f64; 4],
                    curr: bytes[0],
                    prev: bytes[7], // treat the code as 'circular'
                    addr: 0,
                    retval: None,
                },
            )
        }

        fn hashable(&self) -> HashableMachineState {
            let u64_regs: [u64; 4] = self.regs.map(|x| u64::from_be_bytes(x.to_be_bytes()));
            HashableMachineState {
                regs: u64_regs,
                curr: self.curr,
                prev: self.prev,
                addr: self.addr,
                retval: self.retval,
            }
        }
    }
    /*

    Register Operations:
    | _ _ _ | _ | | _ _| | _ _ |
     instr.  op    dest  right


    Arithmetic Operations
    | _ _ _ | _ | | _ _ | _ _ |
      inst.   op   left   right

    Jump Operations:
    | _ _ _ | _ _ _ | _ _ |
      inst.   addr    right



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
    0x7 JMP (CON): jump if right is not zero

    // So one such program to estimate e in a complete fashion:

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

    0101_1111 LOD into r3
    r1 = r3
    r1 = r2
    r1 /= r2
    1011_1010 r2--
    JMP
    1010_0001 r0 += r1
    1001_1111 DEC R3
    JMP

    0101_1111 r3 = some dumb high number
    1000_0000 r0++
    1000_0101 r1++
    1000_1010 r2++
    1101_0110 r1 /= r2
    1010_0001 r0 += r1
    1001_1111 r3--
    1110_1111 jmp 0x3 r3 != 0

    0b01011111_10000000_10000101_10001010_11010110_10100001_10011111_11101111
    */

    fn instr(byte: u8) -> u8 {
        (byte & 0xe0) >> 5
    }

    fn addr(byte: u8) -> usize {
        ((byte & 0x1c) >> 2) as usize
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

    fn execute_byte(mut state: MachineState) -> MachineState {
        // by default, simply go to the next address
        state.addr += 1;

        match instr(state.curr) {
            0b000 => { /* noop */ }
            0b001 => {
                /* return */
                state.retval = Some(r_reg(state.curr));
            }
            0b010 => {
                /* load */
                let v = state.curr & 0b11;
                let dest = &mut state.regs[l_reg(state.curr)];
                if op(state.curr) {
                    //regs[l] = (r as u32) << 8 +
                    *dest = (((v as u32) << 8) + (state.prev as u32)) as f64;
                } else {
                    *dest = v as f64;
                }
            }
            0b011 => {
                /* move | copy */
                state.regs[l_reg(state.curr)] = state.regs[r_reg(state.curr)];
                if op(state.curr) {
                    state.regs[r_reg(state.curr)] = 0f64;
                }
            }
            0b100 => {
                /* inc | dec */
                let l_reg = l_reg(state.curr);
                let r_reg = r_reg(state.curr);
                if op(state.curr) {
                    state.regs[l_reg] = state.regs[r_reg] - 1.0;
                } else {
                    state.regs[l_reg] = state.regs[r_reg] + 1.0;
                }
            }
            0b101 => {
                /* add | sub */
                let l_reg = l_reg(state.curr);
                let r_reg = r_reg(state.curr);
                if op(state.curr) {
                    state.regs[l_reg] -= state.regs[r_reg];
                } else {
                    state.regs[l_reg] += state.regs[r_reg];
                }
            }
            0b110 => {
                /* mul | div */
                let l_reg = l_reg(state.curr);
                let r_reg = r_reg(state.curr);
                if op(state.curr) {
                    state.regs[l_reg] /= state.regs[r_reg];
                } else {
                    state.regs[l_reg] *= state.regs[r_reg];
                }
            }
            0b111 => {
                if state.regs[r_reg(state.curr)] != 0.0 {
                    state.addr = addr(state.curr);
                }
            }
            _ => unreachable!(),
        }
        state
    }

    pub fn execute_code(code: u64) -> f64 {
        let (bytes, mut state) = MachineState::init(code);

        let mut jump_log = HashSet::new();

        let MAX_CYCLES = std::f32::INFINITY;
        let mut cycles = 0.0;
        loop {
            state = execute_byte(state);
            if let Some(retval) = state.retval {
                return state.regs[retval];
            }

            cycles += 1.0;
            if cycles == MAX_CYCLES {
                break;
            }

            if state.addr > 0x7 {
                break;
            }

            if instr(state.curr) == 0x7 && !jump_log.insert(state.hashable()) {
                break;
            }

            // set up for next run
            state.prev = state.curr;
            state.curr = bytes[state.addr];
        }

        state.regs[0]
    }

    pub fn trace_code(code: u64) -> f64 {
        let (bytes, mut state) = MachineState::init(code);

        let mut jump_log = HashSet::new();

        let MAX_CYCLES = std::f32::INFINITY;
        let mut cycles = 0.0;
        loop {
            eprint!("{} 0x{:x}\t", cycles, state.addr);
            state = execute_byte(state);
            eprintln!("{:?}", state.regs);

            if let Some(retval) = state.retval {
                eprintln!("explicit return reg{} [{}]", retval, state.regs[retval]);
                return state.regs[retval];
            }

            if instr(state.curr) == 0x7 && !jump_log.insert(state.hashable()) {
                eprintln!("infinite loop detected");
                break;
            }

            if state.addr > 0x7 {
                break;
            }

            cycles += 1.0;

            // set up for next run
            state.prev = state.curr;
            state.curr = bytes[state.addr];
        }

        eprintln!("implicit return reg0 [{}]", state.regs[0]);
        state.regs[0]
    }

    fn format_byte(byte: u8, prev_byte: u8) -> String {
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
                format!("JMP 0x{:x} if r{} != 0", addr(byte), r_reg)
            }
            _ => String::new(),
        }
    }

    pub fn format_code(code: u64) -> String {
        let bytes = code.to_be_bytes();

        // weird circular trick here
        let mut prev_byte = bytes[7];
        let mut formatted_bytes = Vec::with_capacity(8);
        for (lineno, byte) in bytes.into_iter().enumerate() {
            formatted_bytes.push(format!(
                "0x{:x}\t0x{:0>2x}\t{}",
                lineno,
                byte,
                format_byte(byte, prev_byte)
            ));
            prev_byte = byte;
        }
        formatted_bytes.join("\n")
    }

    #[cfg(test)]
    mod test_code {
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
        fn test_jmp() {
            // try to jmp if r1 is not 0, but it is so you return contents of r0 by default
            assert_code_returns(0b0000_0011_0101_0000_1111_1101_0010_0000_0010_0001, 3.0);
            // same as above, but successfully jump
            assert_code_returns(0b1000_0100_0101_0000_1111_1101_0010_0000_0010_0001, 1.0);
            // sum of numbers 0 to 15
            // NOP: 0000_1111
            // LOD 15 into r1: 0101_0100
            // ADD r0 += r1: 1010_0001
            // DEC r1: 1001_0101
            // JMP if r1 != 0: 1111_0101
            assert_code_returns(0b0000_1111_0101_0100_1010_0001_1001_0101_1111_0101, 120.0);
        }
    }
}
