/*
    All instructions are divided like so:
    | _ _ _ _ | _ _ _ _ | _ _ _ _ | _ _ _ _ |
      opcode.    dest      left      right

    left|right refers to a concatenation of those bits.

    REGISTER OPERATIONS
    0x0 NOP - no operation
    0x1 MOV - move content of right to dest.
        given LEFT && 0b0001:
        - 0b0000 -> clear right (MOV)
        - 0b0001 -> leave right alone (CPY)
    0x2 INT - initialize dest with value in left|right as i8
    0x3 NOP - (reserved for loading byte at address)
    0x4 NOP - (reserved for storing byte at address)

    ARITHMETIC OPERATIONS
    0x5 UNY - perform unary operation on right based on left, store in dest
        given LEFT && 0b0011:
        - 0b0000 -> zero
        - 0b0001 -> inc
        - 0b0010 -> dec
        - 0b0011 -> not
    0x6 ADD - add left and right, store the value in dest
    0x7 SUB - subtract left and right, store the value in dest
    0x8 MUL - multiply left and right, store the value in dest
    0x9 DIV - divide left and right, store the value in dest
    0xa AND - boolean AND (1 if both not zero, 0 otherwise), store in dest
    0xb OR  - boolean OR (1 if either not zero, 1 otherwise), store in dest

    CONTROL FLOW
    0xc BEQ - conditional jump to dest if left and right are equal
    0xd BNE - conditional jump to dest if left and right are not equal
    0xe JMP - unconditional jump to dest
    0xf FLG - flag a register [dest] for return
        - LEFT && 0b0001 == 0b0001 -> return immediately
        - do not return otherwise

*/

#[derive(Default)]
pub struct MachineParams {
    pub max_cycles: Option<usize>,
    pub verbose: bool,
}

#[derive(Default)]
struct MachineState {
    regs: [f64; 16],
    addr: usize,
    flag: usize,
    to_return: bool,
}
struct HashableMachineState([u64; 16], usize, usize);

impl MachineState {
    fn hashable(&self) -> HashableMachineState {
        HashableMachineState(
            self.regs.map(|x| u64::from_be_bytes(x.to_be_bytes())),
            self.addr,
            self.flag,
        )
    }

    fn retval(&self) -> f64 {
        self.regs[self.flag]
    }
}

fn extract_instr(first_byte: u8, second_byte: u8) -> (usize, usize, usize, usize) {
    (
        ((first_byte >> 4) & 0xf) as usize,
        (first_byte & 0xf) as usize,
        ((second_byte >> 4) & 0xf) as usize,
        (second_byte & 0xf) as usize,
    )
}

const OPNAMES: [&'static str; 16] = [
    "NOP", "MOV", "INT", "NOP", "NOP", "UNARY", "ADD", "SUB", "MUL", "DIV", "AND", "OR", "BEQ",
    "BNE", "JMP", "FLG",
];
const OPSYMS: [&'static str; 16] = [
    "", "", "", "", "", "", "+", "-", "*", "/", "&", "|", "==", "=/=", "", "",
];

fn format_bytes(first: u8, second: u8) -> String {
    let (opcode, dest, left, right) = extract_instr(first, second);

    let op_name = OPNAMES[opcode as usize];
    let op_sym = OPSYMS[opcode as usize];

    if opcode < 0xc && opcode > 0x5 {
        return format!(
            "{} r{:0>2} <- r{:0>2} {} r{:0>2}",
            op_name, dest, left, op_sym, right
        );
    }
    match opcode {
        0x0 => String::from("NOP"),
        0x1 => {
            if opcode & 1 == 0 {
                format!("MOV r{} <- r{}", dest, right)
            } else {
                format!("CPY r{} <- r{}", dest, right)
            }
        }
        02 => {
            format!("INT r{:0>2} <- {}", dest, i8::from_be_bytes([second]))
        }
        // RESERVED FOR FUTURE DEVELOPMENT
        0x3 => String::from("NOP"),
        0x4 => String::from("NOP"),
        // UNARY OPS
        0x5 => match left & 0xf {
            0 => format!("CLR r{:0>2} <- 0", dest),
            1 => format!("INC r{:0>2} <- r{:0>2} + 1", dest, right),
            2 => format!("DEC r{:0>2} <- r{:0>2} - 1", dest, right),
            3 => format!("NOT r{:0>2} <- !r{:0>2}", dest, right),
            _ => unreachable!(),
        },
        0xc => format!("BEQ 0x{:0>2x} if r{:0>2} == r{:0>2}", dest, left, right),
        0xd => format!("BNE 0x{:0>2x} if r{:0>2} != r{:0>2}", dest, left, right),
        0xe => format!("JMP 0x{:0>2x}", dest),
        0xf => {
            if opcode & 1 == 0 {
                format!("FLG r{:0>2}", dest)
            } else {
                format!("RET r{:0>2}", dest)
            }
        }
        _ => unreachable!(),
    }
}

pub fn format_code(code: &[u8]) -> String {
    let mut pieces = Vec::new();
    let mut line = 0;
    while line < code.len() / 2 {
        let first = code[line * 2];
        let second = code[line * 2 + 1];
        pieces.push(format!(
            "0x{:0>2x}\t0x{:0>2x}{:0>2x}\t{}",
            line,
            first,
            second,
            format_bytes(first, second)
        ));
        line += 1
    }
    pieces.join("\n")
}

fn execute_instr(mut state: MachineState, code: &[u8]) -> MachineState {
    let (opcode, dest, left, right) = extract_instr(code[state.addr * 2], code[state.addr * 2 + 1]);
    state.addr += 1;
    match opcode {
        // NOP
        0x0 => {}
        // MOV / CPY
        0x1 => {
            if left & 1 == 0 && dest != right {
                state.regs[dest] = state.regs[right];
                state.regs[right] = 0.0;
            } else {
                state.regs[dest] = state.regs[right];
            }
        }
        // INT
        0x2 => {
            state.regs[dest] = i8::from_be_bytes([code[state.addr * 2 - 1]]) as f64;
        }
        // RESERVED
        0x3 => {}
        0x4 => {}
        // ARITHMETIC
        0x5 => match left & 0x3 {
            0 => state.regs[dest] = 0.0,
            1 => state.regs[dest] = &state.regs[right] + 1.0,
            2 => state.regs[dest] = &state.regs[right] - 1.0,
            3 => state.regs[dest] = if &state.regs[right] == &0.0 { 1.0 } else { 0.0 },
            _ => unreachable!(),
        },
        0x6 => {
            state.regs[dest] = state.regs[left] + state.regs[right];
        }
        0x7 => {
            state.regs[dest] = state.regs[left] - state.regs[right];
        }
        0x8 => {
            state.regs[dest] = state.regs[left] * state.regs[right];
        }
        0x9 => {
            state.regs[dest] = state.regs[left] / state.regs[right];
        }
        0xa => {
            state.regs[dest] = if (state.regs[left] != 0.0) && (state.regs[right] != 0.0) {
                1.0
            } else {
                0.0
            }
        }
        0xb => {
            state.regs[dest] = if (state.regs[left] != 0.0) || (state.regs[right] != 0.0) {
                1.0
            } else {
                0.0
            }
        }
        // CONTROL FLOW
        0xc => {
            if state.regs[left] == state.regs[right] {
                state.addr = dest
            }
        }
        0xd => {
            if state.regs[left] != state.regs[right] {
                state.addr = dest
            }
        }
        0xe => state.addr = dest,
        0xf => {
            state.flag = dest;
            if left & 1 == 1 {
                state.to_return = true;
            }
        }
        _ => unreachable!(),
    }
    state
}

#[derive(PartialEq, Debug)]
pub enum Return {
    Implicit(f64, usize),
    Explicit(f64, usize),
    Timeout(f64, usize),
    LoopDetected(f64, usize),
}

impl Return {
    pub fn value(&self) -> f64 {
        match self {
            Return::Implicit(v, _) => *v,
            Return::Explicit(v, _) => *v,
            Return::Timeout(v, _) => *v,
            Return::LoopDetected(v, _) => *v,
        }
    }
}

pub fn execute_code(code: &[u8]) -> Return {
    execute_code_params(code, &MachineParams::default())
}

pub fn execute_code_params(code: &[u8], params: &MachineParams) -> Return {
    let mut state = MachineState::default();

    let mut cycles = 0;
    loop {
        if state.to_return {
            return Return::Explicit(state.retval(), cycles);
        }
        if state.addr * 2 >= code.len() {
            return Return::Implicit(state.retval(), cycles);
        }
        if params.max_cycles.is_some() && params.max_cycles.unwrap() == cycles {
            return Return::Timeout(state.retval(), cycles);
        }
        if params.verbose {
            println!(
                "{}\t0x{:0>2}\t{: <22}\t{:?}",
                cycles,
                state.addr,
                format_bytes(code[state.addr * 2], code[state.addr * 2 + 1]),
                state.regs
            )
        }

        state = execute_instr(state, code);

        cycles += 1;
    }
}

#[cfg(test)]
mod test_machine16bit {
    use super::*;

    macro_rules! assert_code_result {
        ($left:expr, $right:expr) => {
            assert_eq!(execute_code(&$left.to_be_bytes()).value(), $right);
        };
    }

    macro_rules! assert_code_returns {
        ($left:expr, $right:expr) => {
            assert_eq!(execute_code(&$left.to_be_bytes()), $right);
        };
    }

    #[test]
    fn test_nop() {
        assert_code_result!(0x0_usize, 0.0);
    }

    #[test]
    fn test_unary() {
        // r0++
        assert_code_result!(0x5010_u64, 1.0);
        // r0++++
        assert_code_result!(0x5010_5010_u64, 2.0);
        // r0--
        assert_code_result!(0x5020_u64, -1.0);
        // r0--; r0 = 0
        assert_code_result!(0x5020_5000_u64, 0.0);
        // !r0 should equal 1
        assert_code_result!(0x5030_u64, 1.0);
        // !(++r0) should equal 0
        assert_code_result!(0x5010_5010_5030_u64, 0.0);
        // !(--r0) should also be 0
        assert_code_result!(0x5020_5030_u64, 0.0);
    }

    #[test]
    fn test_mov() {
        // r1 = 2, mov r1 to r0
        assert_code_result!(0x5111_5111_1001_u64, 2.0);
        // r0 = 2, mov r0 to r1 (r0 should clear)
        assert_code_result!(0x5010_5010_1100_u64, 0.0);
        // r0 = 2, cpy r0 to r1 (r0 should NOT clear)
        assert_code_result!(0x5010_5010_1110_u64, 2.0);
        // weird edge case: make sure moving a register to itself doesn't clear
        assert_code_result!(0x5010_5010_1000_u64, 2.0);
    }

    #[test]
    fn test_mul_div() {
        // r1 = 2, r2 = 3, r0 = r2 * r1
        assert_code_result!(0x5111_5111_5211_8012_u128, 6.0);
        assert_code_result!(0x5111_5111_5211_8021_u128, 6.0);
        // r1 = 2, r2 = 3, r0 = r2 / r1
        assert_code_result!(0x5111_5111_5211_9021_u128, 3.0 / 2.0);
    }

    #[test]
    fn test_and_or() {
        // r1 = 2, r2 = 3, r0 = r2 and r1
        assert_code_result!(0x5111_5111_5211_a012_u128, 1.0);
        assert_code_result!(0x5111_5111_5211_a021_u128, 1.0);
        // r1 = 2, r2 = 0.0, r0 = r2 and r1
        assert_code_result!(0x5111_5111_a021_u128, 0.0);
        // r1 = 0.0, r2 = 0.0, r0 = r2 and r1
        assert_code_result!(0xa021_u128, 0.0);
        // r1 = 2, r2 = 3, r0 = r2 or r1
        assert_code_result!(0x5111_5111_5211_b012_u128, 1.0);
        assert_code_result!(0x5111_5111_5211_b021_u128, 1.0);
        // r1 = 2, r2 = 0.0, r0 = r2 or r1
        assert_code_result!(0x5111_5111_b021_u128, 1.0);
        // r1 = 0.0, r2 = 0.0, r0 = r2 or r1
        assert_code_result!(0xb021_u128, 0.0);
    }

    #[test]
    fn test_flag_ret() {
        // flag r1, r1 = 2
        assert_code_returns!(0xf100_5111_5111_u128, Return::Implicit(2.0, 8));
        // r1 = 2, flag r1
        assert_code_returns!(0x5111_5111_f100_u128, Return::Implicit(2.0, 8));
        // r1 = 2, ret r1
        assert_code_returns!(0x5111_5111_f110_u128, Return::Explicit(2.0, 8));
        // ret r1, r1 = 2
        assert_code_returns!(0xf110_5111_5111_u128, Return::Explicit(0.0, 6));
    }

    #[test]
    fn test_jmp() {
        // jumping to skip an addition
        assert_code_returns!(0x5010_e700_5010_5010_u128, Return::Implicit(2.0, 7));
        // jumping out of bounds from a small byte slice should simply trigger implicit return
        assert_code_returns!(0x5010_ef00_5010_u64, Return::Implicit(1.0, 3));
        // test an infinite loop with some sort of max on the machine
        assert_eq!(
            execute_code_params(
                &[0xe0, 00],
                &MachineParams {
                    max_cycles: Some(100),
                    verbose: false
                }
            ),
            Return::Timeout(0.0, 100)
        );
    }

    #[test]
    fn test_branch() {
        // r1++; r1 = r1 * r1; r0 = 1 while(r1 != 0) r0 *= r1--;
        assert_code_result!(0x5111_5111_8111_5030_8001_5121_d51f_u128, 24.0);
        // r1++; r1 = r1 * r1; r0 = 1 while(!(r1) == 0) r0 *= r1--;
        assert_code_result!(0x5111_5111_8111_5030_8001_5121_5231_c42f_u128, 24.0);
    }

    #[test]
    fn test_int() {
        assert_code_result!(0x2011_u32, 17.0);
        // it's two's complement so...
        assert_code_result!(0x20ff_u32, -1.0);
    }
}
