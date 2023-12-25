use hashbrown::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    Adc, // ADd with Carry
    And, // bitwise AND with accumulator
    Asl, // Arithmetic Shift Left
    Bit, // test BITs

    // Branch instructions:
    Bpl, // Branch on PLus
    Bmi, // Branch on MInus
    Bvc, // Branch on oVerflow Clear
    Bvs, // Branch on oVerflow Set
    Bcc, // Branch on Carry Clear
    Bcs, // Branch on Carry Set
    Bne, // Branch on Not Equal
    Beq, // Branch on EQual

    Brk, // BReaK
    Cmp, // CoMPare accumulator
    Cpx, // ComPare X register
    Cpy, // ComPare Y register
    Dec, // DECrement memory
    Eor, // bitwise Exclusive OR

    // Flag (Processor Status) instructions:
    Clc, // CLear Carry
    Sec, // SEt Carry
    Cli, // CLear Interrupt
    Sei, // SEt Interrupt
    Clv, // CLear oVerflow
    Cld, // CLear Decimal
    Sed, // SEt Decimal

    Inc, // INCrement memory
    Jmp, // JuMP
    Jsr, // Jump to SubRoutine
    Lda, // LoaD Accumulator
    Ldx, // LoaD X register
    Ldy, // LoaD Y register
    Lsr, // Logical Shift Right
    Nop, // No OPeration
    Ora, // bitwise OR with accumulator

    // Register instructions:
    Tax, // Transfer A to X
    Txa, // Transfer X to A
    Dex, // DEcrement X
    Inx, // INcrement X
    Tay, // Transfer A to Y
    Tya, // Transfer Y to A
    Dey, // Decrement Y
    Iny, // Increment Y

    Rol, // ROtate Left
    Ror, // ROtate Right
    Rti, // ReTurn from Interrupt
    Rts, // ReTurn from Subroutine
    Sbc, // SuBtract with Carry
    Sta, // STore with Accumulator

    // Stack instructions:
    Txs, // Transfer X to Stack ptr
    Tsx, // Transfer Stack ptr to X
    Pha, // PusH Accumulator
    Pla, // PuLl Accumulator
    Php, // PusH Processor status
    Plp, // PuLl Processor status

    Stx, // STore X register
    Sty, // STore Y register
}

#[derive(Debug, Default, Copy, Clone)]
pub enum Operand {
    Imm(u8),  // Immediate
    Zpg(u8),  // ZeroPage
    Zpx(u8),  // ZeroPage,X
    Abs(u16), // Absolute
    Abx(u16), // Absolute,X
    Aby(u16), // Absolute,Y
    Idx(u8),  // Indirect,X
    Idy(u8),  // Indirect,Y
    #[default]
    Nil,
}

pub type Inst = (Opcode, Operand);

pub fn assemble(insts: &[Inst]) -> Vec<u8> {
    use Operand::*;
    let mut bytes = Vec::new();
    for (opcode, operand) in insts {
        match *operand {
            Nil => bytes.push(
                IMMEDIATES
                    .get(opcode)
                    .copied()
                    .unwrap_or_else(|| panic!("No entry found for opcode {opcode:?}")),
            ),
            Imm(x) => bytes.extend([IMMEDIATES[opcode], x]),
            Zpg(x) => bytes.extend([ZERO_PAGES[opcode], x]),
            Zpx(x) => bytes.extend([ZERO_PAGES_X[opcode], x]),
            Abs(x) => bytes.extend(with_16bit_operand(ABSOLUTES[opcode], x)),
            Abx(x) => bytes.extend(with_16bit_operand(ABSOLUTES_X[opcode], x)),
            Aby(x) => bytes.extend(with_16bit_operand(ABSOLUTES_Y[opcode], x)),
            Idx(x) => bytes.extend([INDIRECTS_X[opcode], x]),
            Idy(x) => bytes.extend([INDIRECTS_Y[opcode], x]),
        }
    }
    bytes
}

fn with_16bit_operand(opcode: u8, value: u16) -> Vec<u8> {
    vec![opcode, (value >> 8) as u8, (value & 0xff) as u8]
}

struct HexOpcodes {
    imm: u8,
    zpg: u8,
    zpx: u8,
    abs: u8,
    abx: u8,
    aby: u8,
    idx: u8,
    idy: u8,
}

impl HexOpcodes {
    const fn new(imm: u8, zpg: u8, zpx: u8, abs: u8, abx: u8, aby: u8, idx: u8, idy: u8) -> Self {
        Self {
            imm,
            zpg,
            zpx,
            abs,
            abx,
            aby,
            idx,
            idy,
        }
    }
}

macro_rules! opcode {
    ($opcode:ident, $imm:literal, $zpg:literal, $zpx:literal, $abs:literal, $abx:literal, $aby:literal, $idx:literal, $idy:literal) => {
        (
            Opcode::$opcode,
            HexOpcodes::new($imm, $zpg, $zpx, $abs, $abx, $aby, $idx, $idy),
        )
    };
    ($opcode:ident, $hex:literal) => {
        (Opcode::$opcode, HexOpcodes::new($hex, 0, 0, 0, 0, 0, 0, 0))
    };
}

const OPCODES: [(Opcode, HexOpcodes); 55] = [
    opcode!(Adc, 0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71),
    opcode!(And, 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31),
    opcode!(Bit, 0, 0x24, 0, 0x2c, 0, 0, 0, 0),
    opcode!(Bpl, 0x10),
    opcode!(Bmi, 0x30),
    opcode!(Bvc, 0x50),
    opcode!(Bvs, 0x70),
    opcode!(Bcc, 0x90),
    opcode!(Bcs, 0xb0),
    opcode!(Bne, 0xd0),
    opcode!(Beq, 0xf0),
    opcode!(Brk, 0x00),
    opcode!(Cmp, 0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1),
    opcode!(Cpx, 0xe0, 0xe4, 0, 0xec, 0, 0, 0, 0),
    opcode!(Cpy, 0xc0, 0xc4, 0, 0xcc, 0, 0, 0, 0),
    opcode!(Dec, 0, 0xc6, 0xd6, 0xce, 0xde, 0, 0, 0),
    opcode!(Eor, 0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51),
    opcode!(Clc, 0x18),
    opcode!(Sec, 0x38),
    opcode!(Cli, 0x58),
    opcode!(Sei, 0x78),
    opcode!(Clv, 0xb8),
    opcode!(Cld, 0xd8),
    opcode!(Sed, 0xf8),
    opcode!(Inc, 0, 0xe6, 0xf6, 0xee, 0xfe, 0, 0, 0),
    opcode!(Jmp, 0, 0, 0, 0x4c, 0, 0, 0x6c, 0),
    opcode!(Jsr, 0, 0, 0, 0x20, 0, 0, 0, 0),
    opcode!(Lda, 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1),
    opcode!(Ldx, 0xa2, 0xa6, 0xb6, 0xae, 0, 0xbe, 0, 0),
    opcode!(Ldy, 0xa0, 0xa4, 0xac, 0xbc, 0, 0, 0, 0),
    opcode!(Lsr, 0x4a, 0x46, 0x56, 0x4e, 0x5e, 0, 0, 0),
    opcode!(Nop, 0xea, 0, 0, 0, 0, 0, 0, 0),
    opcode!(Ora, 0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11),
    opcode!(Tax, 0xaa),
    opcode!(Txa, 0x8a),
    opcode!(Dex, 0xca),
    opcode!(Inx, 0xe8),
    opcode!(Tay, 0xa8),
    opcode!(Tya, 0x98),
    opcode!(Dey, 0x88),
    opcode!(Iny, 0xc8),
    opcode!(Rol, 0x2a, 0x26, 0x36, 0x2e, 0x3e, 0, 0, 0),
    opcode!(Ror, 0x6a, 0x66, 0x76, 0x6e, 0x7e, 0, 0, 0),
    opcode!(Rti, 0x40),
    opcode!(Rts, 0x60),
    opcode!(Sbc, 0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1),
    opcode!(Sta, 0, 0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91),
    opcode!(Txs, 0x9a),
    opcode!(Tsx, 0xba),
    opcode!(Pha, 0x48),
    opcode!(Pla, 0x68),
    opcode!(Php, 0x08),
    opcode!(Plp, 0x28),
    opcode!(Stx, 0, 0x86, 0x96, 0x8e, 0, 0, 0, 0), // ZeroPageX is ZeroPageY here
    opcode!(Sty, 0, 0x84, 0x94, 0x8c, 0, 0, 0, 0),
];

macro_rules! opcodes_by_mode {
    ($mode:ident) => {
        OPCODES
            .into_iter()
            .map(|(opcode, hexcodes)| (opcode, hexcodes.$mode))
            .collect()
    };
}

lazy_static! {
    static ref IMMEDIATES: HashMap<Opcode, u8> = opcodes_by_mode!(imm);
    static ref ZERO_PAGES: HashMap<Opcode, u8> = opcodes_by_mode!(zpg);
    static ref ZERO_PAGES_X: HashMap<Opcode, u8> = opcodes_by_mode!(zpx);
    static ref ABSOLUTES: HashMap<Opcode, u8> = opcodes_by_mode!(abs);
    static ref ABSOLUTES_X: HashMap<Opcode, u8> = opcodes_by_mode!(abx);
    static ref ABSOLUTES_Y: HashMap<Opcode, u8> = opcodes_by_mode!(aby);
    static ref INDIRECTS_X: HashMap<Opcode, u8> = opcodes_by_mode!(idx);
    static ref INDIRECTS_Y: HashMap<Opcode, u8> = opcodes_by_mode!(idy);
}

pub fn hexdump(bytes: &[u8]) {
    for line in bytes.chunks(8) {
        println!(
            "{}",
            line.iter()
                .map(|b| format!("{b:02x}"))
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_assemble_prog() {
        use super::{assemble, Opcode, Operand};
        let prog = vec![
            (Opcode::Lda, Operand::Imm(0xc0)),
            (Opcode::Tax, Operand::Nil),
            (Opcode::Inx, Operand::Nil),
            (Opcode::Adc, Operand::Imm(0xc4)),
            (Opcode::Brk, Operand::Nil),
        ];
        assert_eq!(
            assemble(&prog),
            vec![0xa9, 0xc0, 0xaa, 0xe8, 0x69, 0xc4, 0x00]
        );
    }
}
