use bitflags::bitflags;
use int_enum::IntEnum;

pub struct Interp {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    memory: [u8; 65536],
    flags: Flags,
}

impl Default for Interp {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: 0,
            memory: [0u8; 65536],
            flags: Flags::default(),
        }
    }
}

impl Interp {
    /// Runs the instructions, returning the value in the A register
    /// or a `Signal` if the program triggers an interrupt.
    pub fn interp(prog: &[u8]) -> Result<Self, Signal> {
        let mut interp = Interp::default();
        interp.load(prog);
        interp.run()?;
        Ok(interp)
    }

    pub fn load(&mut self, program: &[u8]) {
        let start_addr = 0x8000;
        let reset_vector = 0xfffc;

        for (i, &byte) in program.iter().enumerate() {
            self.memory[start_addr + i] = byte;
        }

        self.memory[reset_vector] = (start_addr & 0xff) as u8;
        self.memory[reset_vector + 1] = (start_addr >> 8) as u8;

        self.pc = start_addr as u16;
    }

    pub fn run(&mut self) -> Result<(), Signal> {
        loop {
            use Opcode::*;
            match Opcode::from_int(self.take_byte()).unwrap() {
                ADCImm => self.a = self.a.wrapping_add(self.take_imm()),
                ADCZpg => self.a = self.a.wrapping_add(self.memory[self.take_zpg() as usize]),
                ADCZpx => self.a = self.a.wrapping_add(self.memory[self.take_zpx() as usize]),
                ADCAbs => self.a = self.a.wrapping_add(self.memory[self.take_abs() as usize]),
                ADCAbx => self.a = self.a.wrapping_add(self.memory[self.take_abx() as usize]),
                ADCAby => self.a = self.a.wrapping_add(self.memory[self.take_aby() as usize]),
                ADCInx => self.a = self.a.wrapping_add(self.memory[self.take_inx() as usize]),
                ADCIny => self.a = self.a.wrapping_add(self.memory[self.take_iny() as usize]),

                ANDImm => self.a &= self.take_imm(),
                ANDZpg => self.a &= self.memory[self.take_zpg() as usize],
                ANDZpx => self.a &= self.memory[self.take_zpx() as usize],
                ANDAbs => self.a &= self.memory[self.take_abs() as usize],
                ANDAbx => self.a &= self.memory[self.take_abx() as usize],
                ANDAby => self.a &= self.memory[self.take_aby() as usize],
                ANDInx => self.a &= self.memory[self.take_inx() as usize],
                ANDIny => self.a &= self.memory[self.take_iny() as usize],

                ASLAcc => todo!(),
                ASLZpg => {
                    let addr = self.take_zpg() as u16;
                    self.op_asl(addr);
                }
                ASLZpx => {
                    let addr = self.take_zpx() as u16;
                    self.op_asl(addr);
                }
                ASLAbs => {
                    let addr = self.take_abs();
                    self.op_asl(addr);
                }
                ASLAbx => {
                    let addr = self.take_abx();
                    self.op_asl(addr);
                }

                BITZpg => {
                    let addr = self.take_zpg() as u16;
                    self.op_bit(addr);
                }
                BITAbs => {
                    let addr = self.take_abs();
                    self.op_bit(addr);
                }

                BPL => {
                    let cond = !self.flags.contains(Flags::Negative);
                    self.branch(cond);
                }
                BMI => {
                    let cond = self.flags.contains(Flags::Negative);
                    self.branch(cond);
                }
                BVC => {
                    let cond = !self.flags.contains(Flags::Overflow);
                    self.branch(cond);
                }
                BVS => {
                    let cond = self.flags.contains(Flags::Overflow);
                    self.branch(cond);
                }
                BCC => {
                    let cond = !self.flags.contains(Flags::Carry);
                    self.branch(cond);
                }
                BCS => {
                    let cond = self.flags.contains(Flags::Carry);
                    self.branch(cond);
                }
                BNE => {
                    let cond = !self.flags.contains(Flags::Zero);
                    self.branch(cond);
                }
                BEQ => {
                    let cond = self.flags.contains(Flags::Zero);
                    self.branch(cond);
                }

                BRK => break,

                CMPImm => {
                    let val = self.take_imm();
                    self.op_cmp(self.a, val);
                }
                CMPZpg => {
                    let val = self.take_zpg();
                    self.op_cmp(self.a, val);
                }
                CMPZpx => {
                    let val = self.memory[self.take_zpx() as usize];
                    self.op_cmp(self.a, val);
                }
                CMPAbs => {
                    let val = self.memory[self.take_abs() as usize];
                    self.op_cmp(self.a, val);
                }
                CMPAbx => {
                    let val = self.memory[self.take_abx() as usize];
                    self.op_cmp(self.a, val);
                }
                CMPAby => {
                    let val = self.memory[self.take_aby() as usize];
                    self.op_cmp(self.a, val);
                }
                CMPInx => {
                    let val = self.memory[self.take_inx() as usize];
                    self.op_cmp(self.a, val);
                }
                CMPIny => {
                    let val = self.memory[self.take_iny() as usize];
                    self.op_cmp(self.a, val);
                }

                CPXImm => {
                    let val = self.take_imm();
                    self.op_cmp(self.x, val);
                }
                CPXZpg => {
                    let val = self.memory[self.take_zpg() as usize];
                    self.op_cmp(self.x, val);
                }
                CPXAbs => {
                    let val = self.memory[self.take_abs() as usize];
                    self.op_cmp(self.x, val);
                }

                CPYImm => {
                    let val = self.take_imm();
                    self.op_cmp(self.y, val);
                }
                CPYZpg => {
                    let val = self.memory[self.take_zpg() as usize];
                    self.op_cmp(self.y, val);
                }
                CPYAbs => {
                    let val = self.memory[self.take_abs() as usize];
                    self.op_cmp(self.y, val);
                }

                DECZpg => {
                    let operand = &mut self.memory[self.take_zpg() as usize];
                    *operand = operand.wrapping_sub(1);
                }
                DECZpx => {
                    let operand = &mut self.memory[self.take_zpx() as usize];
                    *operand = operand.wrapping_sub(1);
                }
                DECAbs => {
                    let operand = &mut self.memory[self.take_abs() as usize];
                    *operand = operand.wrapping_sub(1);
                }
                DECAbx => {
                    let operand = &mut self.memory[self.take_abx() as usize];
                    *operand = operand.wrapping_sub(1);
                }

                EORImm => self.a ^= self.take_imm(),
                EORZpg => self.a ^= self.memory[self.take_zpg() as usize],
                EORZpx => self.a ^= self.memory[self.take_zpx() as usize],
                EORAbs => self.a ^= self.memory[self.take_abs() as usize],
                EORAbx => self.a ^= self.memory[self.take_abx() as usize],
                EORAby => self.a ^= self.memory[self.take_aby() as usize],
                EORInx => self.a ^= self.memory[self.take_inx() as usize],
                EORIny => self.a ^= self.memory[self.take_iny() as usize],

                CLC => self.flags.remove(Flags::Carry),
                SEC => self.flags.insert(Flags::Carry),
                CLI => self.flags.remove(Flags::InterruptDisable),
                SEI => self.flags.insert(Flags::InterruptDisable),
                CLV => self.flags.remove(Flags::Overflow),
                CLD => self.flags.remove(Flags::DecimalMode),
                SED => self.flags.insert(Flags::DecimalMode),

                INCZpg => {
                    let operand = &mut self.memory[self.take_zpg() as usize];
                    *operand = operand.wrapping_add(1);
                }
                INCZpx => {
                    let operand = &mut self.memory[self.take_zpx() as usize];
                    *operand = operand.wrapping_add(1);
                }
                INCAbs => {
                    let operand = &mut self.memory[self.take_abs() as usize];
                    *operand = operand.wrapping_add(1);
                }
                INCAbx => {
                    let operand = &mut self.memory[self.take_abx() as usize];
                    *operand = operand.wrapping_add(1);
                }

                JMPAbs => self.pc = self.take_word(),
                JMPInd => {
                    let ptr_addr = self.take_word();

                    let low_byte = self.memory[ptr_addr as usize];
                    let high_byte = if ptr_addr & 0x00ff == 0x00ff {
                        self.memory[(ptr_addr & 0x00ff) as usize]
                    } else {
                        self.memory[(ptr_addr + 1) as usize]
                    };

                    self.pc = (high_byte as u16) << 8 | (low_byte as u16);
                }

                JSR => {
                    let target_addr = self.take_word();
                    let return_addr = self.pc.wrapping_sub(1);

                    self.push_stack((return_addr >> 8) as u8);
                    self.push_stack(return_addr as u8);

                    self.pc = target_addr;
                }

                LDAImm => self.a = self.take_imm(),
                LDAZpg => self.a = self.memory[self.take_zpg() as usize],
                LDAZpx => self.a = self.memory[self.take_zpx() as usize],
                LDAAbs => self.a = self.memory[self.take_abs() as usize],
                LDAAbx => self.a = self.memory[self.take_abx() as usize],
                LDAAby => self.a = self.memory[self.take_aby() as usize],
                LDAInx => self.a = self.memory[self.take_inx() as usize],
                LDAIny => self.a = self.memory[self.take_iny() as usize],

                LDXImm => self.x = self.take_imm(),
                LDXZpg => self.x = self.memory[self.take_zpg() as usize],
                LDXZpy => self.x = self.memory[self.take_zpy() as usize],
                LDXAbs => self.x = self.memory[self.take_abs() as usize],
                LDXAby => self.x = self.memory[self.take_aby() as usize],

                LDYImm => self.y = self.take_imm(),
                LDYZpg => self.y = self.memory[self.take_zpg() as usize],
                LDYZpx => self.y = self.memory[self.take_zpx() as usize],
                LDYAbs => self.y = self.memory[self.take_abs() as usize],
                LDYAbx => self.y = self.memory[self.take_abx() as usize],

                LSRAcc => {
                    let operand = self.take_imm();
                    let result = operand >> 1;
                    self.flags.set(Flags::Carry, operand & 0x80 != 0);
                    self.set_zero_and_negative_flags(result);
                    self.a = result;
                }
                LSRZpg => {
                    let addr = self.take_zpg() as u16;
                    self.op_lsr(addr);
                }
                LSRZpx => {
                    let addr = self.take_zpx() as u16;
                    self.op_lsr(addr);
                }
                LSRAbs => {
                    let addr = self.take_abs();
                    self.op_lsr(addr);
                }
                LSRAbx => {
                    let addr = self.take_abx();
                    self.op_lsr(addr);
                }

                NOP => {}

                ORAImm => self.a |= self.take_imm(),
                ORAZpg => self.a |= self.memory[self.take_zpg() as usize],
                ORAZpx => self.a |= self.memory[self.take_zpx() as usize],
                ORAAbs => self.a |= self.memory[self.take_abs() as usize],
                ORAAbx => self.a |= self.memory[self.take_abx() as usize],
                ORAAby => self.a |= self.memory[self.take_aby() as usize],
                ORAInx => self.a |= self.memory[self.take_inx() as usize],
                ORAIny => self.a |= self.memory[self.take_iny() as usize],

                TAX => self.x = self.a,
                TXA => self.a = self.x,
                DEX => self.x -= 1,
                INX => self.x += 1,
                TAY => self.y = self.a,
                TYA => self.a = self.y,
                DEY => self.y -= 1,
                INY => self.y += 1,

                ROLAcc => self.a = self.op_rol(self.a),
                ROLZpg => {
                    let addr = self.take_zpg() as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                }
                ROLZpx => {
                    let addr = self.take_zpx() as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                }
                ROLAbs => {
                    let addr = self.take_abs() as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                }
                ROLAbx => {
                    let addr = self.take_abx() as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                }

                RORAcc => self.a = self.op_ror(self.a),
                RORZpg => {
                    let addr = self.take_zpg() as usize;
                    self.memory[addr] = self.op_ror(self.memory[addr]);
                }
                RORZpx => {
                    let addr = self.take_zpx() as usize;
                    self.memory[addr] = self.op_ror(self.memory[addr]);
                }
                RORAbs => {
                    let addr = self.take_abs() as usize;
                    self.memory[addr] = self.op_ror(self.memory[addr]);
                }
                RORAbx => {
                    let addr = self.take_abx() as usize;
                    self.memory[addr] = self.op_ror(self.memory[addr]);
                }

                RTI => {
                    // Pull Processor Status Word from stack
                    self.sp = self.sp.wrapping_add(1);
                    let status = self.memory[self.sp as usize];
                    self.flags = Flags::from_bits_truncate(status);

                    // Pull Program Counter from stack (low byte, then high byte)
                    self.sp = self.sp.wrapping_add(1);
                    let low_byte = self.memory[self.sp as usize] as u16;
                    self.sp = self.sp.wrapping_add(1);
                    let high_byte = self.memory[self.sp as usize] as u16;

                    self.pc = (high_byte << 8) | low_byte;
                }

                RTS => {
                    // Pull Return Address from stack (low byte, then high byte)
                    self.sp = self.sp.wrapping_add(1);
                    let low_byte = self.memory[self.sp as usize] as u16;
                    self.sp = self.sp.wrapping_add(1);
                    let high_byte = self.memory[self.sp as usize] as u16;

                    let return_address = ((high_byte << 8) | low_byte).wrapping_add(1);

                    self.pc = return_address;
                }

                SBCImm => self.a = self.a.wrapping_sub(self.take_imm()),
                SBCZpg => self.a = self.a.wrapping_sub(self.memory[self.take_zpg() as usize]),
                SBCZpx => self.a = self.a.wrapping_sub(self.memory[self.take_zpx() as usize]),
                SBCAbs => self.a = self.a.wrapping_sub(self.memory[self.take_abs() as usize]),
                SBCAbx => self.a = self.a.wrapping_sub(self.memory[self.take_abx() as usize]),
                SBCAby => self.a = self.a.wrapping_sub(self.memory[self.take_aby() as usize]),
                SBCInx => self.a = self.a.wrapping_sub(self.memory[self.take_inx() as usize]),
                SBCIny => self.a = self.a.wrapping_sub(self.memory[self.take_iny() as usize]),

                STAZpg => self.a = self.memory[self.take_zpg() as usize],
                STAZpx => self.a = self.memory[self.take_zpx() as usize],
                STAAbs => self.a = self.memory[self.take_abs() as usize],
                STAAbx => self.a = self.memory[self.take_abx() as usize],
                STAAby => self.a = self.memory[self.take_aby() as usize],
                STAInx => self.a = self.memory[self.take_inx() as usize],
                STAIny => self.a = self.memory[self.take_iny() as usize],

                TXS => self.sp = self.x,
                TSX => {
                    self.x = self.sp;
                    self.set_zero_and_negative_flags(self.x);
                }
                PHA => self.push_stack(self.a),
                PLA => {
                    self.a = self.pull_stack();
                    self.set_zero_and_negative_flags(self.a);
                }
                PHP => self.push_stack(self.flags.bits()),
                PLP => self.flags = Flags::from_bits_truncate(self.pull_stack()),

                STXZpg => self.x = self.memory[self.take_zpg() as usize],
                STXZpy => self.x = self.memory[self.take_zpy() as usize],
                STXAbs => self.x = self.memory[self.take_abs() as usize],

                STYZpg => self.x = self.memory[self.take_zpg() as usize],
                STYZpx => self.x = self.memory[self.take_zpx() as usize],
                STYAbs => self.x = self.memory[self.take_abs() as usize],
            }
        }
        Ok(())
    }

    pub fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.flags.set(Flags::Zero, value == 0);
        self.flags.set(Flags::Negative, (value & (1 << 7)) != 0);
    }

    pub fn push_stack(&mut self, value: u8) {
        self.memory[self.sp as usize] = value;
        self.sp = self.sp.wrapping_sub(1);
    }

    pub fn pull_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_sub(1);
        self.memory[self.sp as usize]
    }

    /// Runs the ASL instruction and updates corresponding flags.
    #[inline]
    fn op_asl(&mut self, addr: u16) {
        let operand = self.memory[addr as usize];
        let result = operand << 1;
        self.flags.set(Flags::Carry, operand & 0x80 != 0);
        self.set_zero_and_negative_flags(result);
        self.memory[addr as usize] = result;
    }

    /// Runs the LSR instruction and updates corresponding flags.
    #[inline]
    fn op_lsr(&mut self, addr: u16) {
        let operand = self.memory[addr as usize];
        let result = operand >> 1;
        self.flags.set(Flags::Carry, operand & 0x80 != 0);
        self.set_zero_and_negative_flags(result);
        self.memory[addr as usize] = result;
    }

    #[inline]
    fn op_bit(&mut self, addr: u16) {
        let value = self.memory[addr as usize];
        let acc = self.a;
        self.flags.set(Flags::Zero, (value & acc) == 0);
        self.flags.set(Flags::Negative, value & 0x80 != 0);
        self.flags.set(Flags::Overflow, value & 0x40 != 0);
    }

    #[inline]
    fn branch(&mut self, condition: bool) {
        let displacement = self.take_byte() as i8;

        if condition {
            let pc = self.pc;
            let new_pc = pc.wrapping_add(displacement as u16);
            self.pc = new_pc;

            // TODO: handle additional cycles for page boundary crossing
        }
    }

    #[inline]
    fn op_cmp(&mut self, acc: u8, value: u8) {
        let result = acc.wrapping_sub(value);

        self.flags.set(Flags::Carry, acc >= value);
        self.flags.set(Flags::Zero, result == 0);
        self.flags.set(Flags::Negative, value & 0x80 != 0);
    }

    #[inline]
    fn op_rol(&mut self, value: u8) -> u8 {
        let carry = self.flags.contains(Flags::Carry);
        self.flags.set(Flags::Carry, value & 0x80 != 0); // Set carry flag based on old bit 7
        let result = (value << 1) | (carry as u8);
        self.set_zero_and_negative_flags(result);
        result
    }

    #[inline]
    fn op_ror(&mut self, value: u8) -> u8 {
        let carry = self.flags.contains(Flags::Carry);
        self.flags.set(Flags::Carry, value & 0x01 != 0); // Set carry flag based on old bit 0
        let result = (value >> 1) | ((carry as u8) << 7);
        self.set_zero_and_negative_flags(result);
        result
    }

    /// Takes the byte store at memory[pc] and increments pc.
    #[inline]
    fn take_byte(&mut self) -> u8 {
        let b = self.memory[self.pc as usize];
        self.pc += 1;
        b
    }

    /// Joins the bytes stored at memory[pc..pc+1] and increments pc by two.
    #[inline]
    fn take_word(&mut self) -> u16 {
        let low = self.memory[self.pc as usize] as u16;
        let high = self.memory[(self.pc + 1) as usize] as u16;
        self.pc += 2;
        (high << 8) | low
    }

    /// Decodes the operand of an instruction with an Immediate operand.
    #[inline]
    fn take_imm(&mut self) -> u8 {
        self.take_byte()
    }

    /// Decodes the operand of an instruction with the Zero Page operand.
    #[inline]
    fn take_zpg(&mut self) -> u8 {
        let addr = self.take_byte();
        addr
    }

    /// Decodes the operand of an instruction with the Zero Page,X operand.
    #[inline]
    fn take_zpx(&mut self) -> u8 {
        let addr = self.take_byte();
        let offs = self.x;
        addr + offs
    }

    /// Decodes the operand of an instruction with the Zero Page,Y operand.
    #[inline]
    fn take_zpy(&mut self) -> u8 {
        let addr = self.take_byte();
        let offs = self.y;
        addr + offs
    }

    #[inline]
    fn take_abs(&mut self) -> u16 {
        let a = self.take_byte() as u16;
        let b = self.take_byte() as u16;
        let addr = (b << 8) | a;
        addr
    }

    #[inline]
    fn take_abx(&mut self) -> u16 {
        let a = self.take_byte() as u16;
        let b = self.take_byte() as u16;
        let addr = (b << 8) | a;
        let offs = self.x as u16;
        addr + offs
    }

    #[inline]
    fn take_aby(&mut self) -> u16 {
        let a = self.take_byte() as u16;
        let b = self.take_byte() as u16;
        let addr = (b << 8) | a;
        let offs = self.y as u16;
        addr + offs
    }

    #[inline]
    fn take_inx(&mut self) -> u16 {
        let base = self.take_byte();
        let low = self.memory[(base + self.x) as usize] as u16;
        let high = (self.memory[(base + self.x + 1) as usize] as u16) << 8;
        let addr = low | high;
        addr
    }

    #[inline]
    fn take_iny(&mut self) -> u16 {
        let base = self.take_byte();
        let addr = ((self.memory[base as usize] as u16)
            | ((self.memory[(base + 1) as usize] as u16) << 8))
            + self.y as u16;
        addr
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, IntEnum)]
#[repr(u8)]
enum Opcode {
    // ADC (ADd with Carry)
    ADCImm = 0x69,
    ADCZpg = 0x65,
    ADCZpx = 0x75,
    ADCAbs = 0x6d,
    ADCAbx = 0x7d,
    ADCAby = 0x79,
    ADCInx = 0x61,
    ADCIny = 0x71,

    // AND (bitwise AND with accumulator)
    ANDImm = 0x29,
    ANDZpg = 0x25,
    ANDZpx = 0x35,
    ANDAbs = 0x2d,
    ANDAbx = 0x3d,
    ANDAby = 0x39,
    ANDInx = 0x21,
    ANDIny = 0x31,

    // ASL (Arithmetic Shift Left)
    ASLAcc = 0x0a,
    ASLZpg = 0x06,
    ASLZpx = 0x16,
    ASLAbs = 0x0e,
    ASLAbx = 0x1e,

    // BIT (test BITs)
    BITZpg = 0x24,
    BITAbs = 0x2c,

    // Branch Instructions
    BPL = 0x10,
    BMI = 0x30,
    BVC = 0x50,
    BVS = 0x70,
    BCC = 0x90,
    BCS = 0xB0,
    BNE = 0xD0,
    BEQ = 0xF0,

    // BRK (BReaK)
    BRK = 0x00,

    // CMP (CoMPare accumulator)
    CMPImm = 0xc9,
    CMPZpg = 0xc5,
    CMPZpx = 0xd5,
    CMPAbs = 0xcd,
    CMPAbx = 0xdd,
    CMPAby = 0xd9,
    CMPInx = 0xc1,
    CMPIny = 0xd1,

    // CPX (ComPare X register)
    CPXImm = 0xe0,
    CPXZpg = 0xe4,
    CPXAbs = 0xec,

    // CPY (ComPare Y register)
    CPYImm = 0xc0,
    CPYZpg = 0xc4,
    CPYAbs = 0xcc,

    // DEC (DECrement memory)
    DECZpg = 0xc6,
    DECZpx = 0xd6,
    DECAbs = 0xce,
    DECAbx = 0xde,

    // EOR (bitwise Exclusive OR)
    EORImm = 0x49,
    EORZpg = 0x45,
    EORZpx = 0x55,
    EORAbs = 0x4d,
    EORAbx = 0x5d,
    EORAby = 0x59,
    EORInx = 0x41,
    EORIny = 0x51,

    // Flag (Processor Status) Instructions
    CLC = 0x18, // CLear Carry
    SEC = 0x38, // SEt Carry
    CLI = 0x58, // CLear Interrupt
    SEI = 0x78, // SEt Interrupt
    CLV = 0xb8, // CLear oVerflow
    CLD = 0xd8, // CLear Decimal
    SED = 0xf8, // SEt Decimal

    // INC (INCrement memory)
    INCZpg = 0xe6,
    INCZpx = 0xf6,
    INCAbs = 0xee,
    INCAbx = 0xfe,

    // JMP (JuMP)
    JMPAbs = 0x4c,
    JMPInd = 0x6c,

    // JSR (Jump to SubRoutine)
    JSR = 0x20,

    // LDA (LoaD Accumulator)
    LDAImm = 0xa9,
    LDAZpg = 0xa5,
    LDAZpx = 0xb5,
    LDAAbs = 0xad,
    LDAAbx = 0xbd,
    LDAAby = 0xb9,
    LDAInx = 0xa1,
    LDAIny = 0xb1,

    // LDX (LoaD X register)
    LDXImm = 0xa2,
    LDXZpg = 0xa6,
    LDXZpy = 0xb6,
    LDXAbs = 0xae,
    LDXAby = 0xbe,

    // LDY (LoaD Y register)
    LDYImm = 0xa0,
    LDYZpg = 0xa4,
    LDYZpx = 0xb4,
    LDYAbs = 0xac,
    LDYAbx = 0xbc,

    // LSR (Logical Shift Right)
    LSRAcc = 0x4a,
    LSRZpg = 0x46,
    LSRZpx = 0x56,
    LSRAbs = 0x4e,
    LSRAbx = 0x5e,

    // NOP (No OPeration)
    NOP = 0xea,

    // ORA (bitwise OR with Accumulator)
    ORAImm = 0x09,
    ORAZpg = 0x05,
    ORAZpx = 0x15,
    ORAAbs = 0x0d,
    ORAAbx = 0x1d,
    ORAAby = 0x19,
    ORAInx = 0x01,
    ORAIny = 0x11,

    // Register Instructions
    TAX = 0xaa,
    TXA = 0x8a,
    DEX = 0xca,
    INX = 0xe8,
    TAY = 0xa8,
    TYA = 0x98,
    DEY = 0x88,
    INY = 0xc8,

    // ROL (ROtate Left)
    ROLAcc = 0x2a,
    ROLZpg = 0x26,
    ROLZpx = 0x36,
    ROLAbs = 0x2e,
    ROLAbx = 0x3e,

    // ROR (ROtate Right)
    RORAcc = 0x6a,
    RORZpg = 0x66,
    RORZpx = 0x76,
    RORAbs = 0x6e,
    RORAbx = 0x7e,

    // RTI (ReTurn from Interrupt)
    RTI = 0x40,

    // RTS (ReTurn from Subroutine)
    RTS = 0x60,

    // SBC (SuBtract with Carry)
    SBCImm = 0xe9,
    SBCZpg = 0xe5,
    SBCZpx = 0xf5,
    SBCAbs = 0xed,
    SBCAbx = 0xfd,
    SBCAby = 0xf9,
    SBCInx = 0xe1,
    SBCIny = 0xf1,

    // STA (STore Accumulator)
    STAZpg = 0x85,
    STAZpx = 0x95,
    STAAbs = 0x8d,
    STAAbx = 0x9d,
    STAAby = 0x99,
    STAInx = 0x81,
    STAIny = 0x91,

    // Stack Instructions
    TXS = 0x9a, // Transfer X to Stack ptr
    TSX = 0xba, // Transfer Stack ptr to X
    PHA = 0x48, // PusH Accumulator
    PLA = 0x68, // PuLl Accumulator
    PHP = 0x08, // PusH Processor status
    PLP = 0x28, // PuLl Processor status

    // STX (STore X register)
    STXZpg = 0x86,
    STXZpy = 0x96,
    STXAbs = 0x8e,

    // STY (STore Y register)
    STYZpg = 0x84,
    STYZpx = 0x94,
    STYAbs = 0x8c,
}

bitflags! {
    #[derive(Default)]
    struct Flags: u8 {
        const Carry = 1 << 0;
        const Zero = 1 << 1;
        const InterruptDisable = 1 << 2;
        const DecimalMode = 1 << 3;
        const BreakCommand = 1 << 4;
        const Overflow = 1 << 5;
        const Negative = 1 << 6;
    }
}

#[derive(Debug)]
pub enum Signal {}

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
    use super::Interp;

    const TEST_PROG: [u8; 7] = [0xa9, 0xc0, 0xaa, 0xe8, 0x69, 0xc4, 0x00];

    #[test]
    fn test_run_prog() {
        let interp = Interp::interp(&TEST_PROG).unwrap();
        assert_eq!(interp.a, 0xc0_u8.wrapping_add(0xc4));
        assert_eq!(interp.x, 0xc0 + 1);
    }
}
