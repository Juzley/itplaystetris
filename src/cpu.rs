use log::debug;

use crate::defs::{Condition, Interrupt, Operand16, Operand8, Register16, Register8};
use crate::memory::Memory;
use crate::utils::{from_twos_complement, unbounded_shr_u8};

const ZERO_FLAG_SHIFT: u8 = 7;
const NEG_FLAG_SHIFT: u8 = 6;
const HALF_CARRY_FLAG_SHIFT: u8 = 5;
const CARRY_FLAG_SHIFT: u8 = 4;

const JOYPAD_VEC: u16 = 0x0060;
const SERIAL_VEC: u16 = 0x0058;
const TIMER_VEC: u16 = 0x0050;
const LCD_VEC: u16 = 0x0048;
const VBLANK_VEC: u16 = 0x0040;

#[allow(non_snake_case)]
pub struct CPU {
    AF: u16, // Accumulator & Flags
    BC: u16,
    DE: u16,
    HL: u16,
    SP: u16,
    PC: u16,
    enable_interrupts: bool,
}

impl Default for CPU {
    fn default() -> CPU {
        CPU {
            AF: 0,
            BC: 0,
            DE: 0,
            HL: 0,
            SP: 0xFFFE,
            PC: 0,
            enable_interrupts: false,
        }
    }
}

fn r_to_source(r: u8) -> Operand8 {
    match r {
        0 => Operand8::Direct(Register8::B),
        1 => Operand8::Direct(Register8::C),
        2 => Operand8::Direct(Register8::D),
        3 => Operand8::Direct(Register8::E),
        4 => Operand8::Direct(Register8::H),
        5 => Operand8::Direct(Register8::L),
        6 => Operand8::IndirectReg(Register16::HL),
        7 => Operand8::Direct(Register8::A),
        _ => panic!("Invalid source value"),
    }
}

impl CPU {
    fn read_n(&mut self, mem: &Memory) -> u8 {
        return mem.read_8(self.PC + 1);
    }

    fn read_nn(&mut self, mem: &Memory) -> u16 {
        let low = mem.read_8(self.PC + 1) as u16;
        let high = mem.read_8(self.PC + 2) as u16;
        (high << 8) + low
    }

    fn eval_condition(&self, cond: Condition) -> bool {
        match cond {
            Condition::NZ => !self.get_zero_flag(),
            Condition::Z => self.get_zero_flag(),
            Condition::NC => !self.get_carry_flag(),
            Condition::C => self.get_carry_flag(),
        }
    }

    fn set_af(&mut self, acc: u8, zero: bool, neg: bool, half_carry: bool, carry: bool) {
        self.AF = ((acc as u16) << 8)
            + ((zero as u16) << ZERO_FLAG_SHIFT)
            + ((neg as u16) << NEG_FLAG_SHIFT)
            + ((half_carry as u16) << HALF_CARRY_FLAG_SHIFT)
            + ((carry as u16) << CARRY_FLAG_SHIFT);
    }

    fn set_flags(&mut self, zero: bool, neg: bool, half_carry: bool, carry: bool) {
        let acc = ((self.AF & 0xFF00) >> 8) as u8;
        self.set_af(acc, zero, neg, half_carry, carry);
    }

    fn get_carry_flag(&self) -> bool {
        ((self.AF >> CARRY_FLAG_SHIFT) & 0x1) != 0
    }

    fn get_half_carry_flag(&self) -> bool {
        ((self.AF >> HALF_CARRY_FLAG_SHIFT) & 0x1) != 0
    }

    fn get_neg_flag(&self) -> bool {
        ((self.AF >> NEG_FLAG_SHIFT) & 0x1) != 0
    }

    fn get_zero_flag(&self) -> bool {
        ((self.AF >> ZERO_FLAG_SHIFT) & 0x1) != 0
    }

    fn get_reg_8(&self, reg: Register8) -> u8 {
        let res = match reg {
            Register8::A => self.AF >> 8,
            Register8::F => self.AF & 0xFF,
            Register8::B => self.BC >> 8,
            Register8::C => self.BC & 0xFF,
            Register8::D => self.DE >> 8,
            Register8::E => self.DE & 0xFF,
            Register8::H => self.HL >> 8,
            Register8::L => self.HL & 0xFF,
        };
        res as u8
    }

    fn get_reg_16(&self, reg: Register16) -> u16 {
        match reg {
            Register16::AF => self.AF,
            Register16::BC => self.BC,
            Register16::DE => self.DE,
            Register16::HL => self.HL,
            Register16::SP => self.SP,
        }
    }

    fn get_mem_8(&self, mem: &Memory, addr: u16) -> u8 {
        return mem.read_8(addr);
    }

    fn get_mem_16(&self, mem: &Memory, addr: u16) -> u16 {
        return mem.read_16(addr);
    }

    fn get_val_8(&self, mem: &Memory, src: Operand8) -> u8 {
        match src {
            Operand8::Immediate(val) => val,
            Operand8::Direct(reg) => self.get_reg_8(reg),
            Operand8::IndirectAddr(addr) => self.get_mem_8(mem, addr),
            Operand8::IndirectReg(reg) => self.get_mem_8(mem, self.get_reg_16(reg)),
        }
    }

    fn get_val_16(&self, mem: &Memory, src: Operand16) -> u16 {
        match src {
            Operand16::Immediate(val) => val,
            Operand16::Direct(reg) => self.get_reg_16(reg),
            Operand16::IndirectAddr(addr) => self.get_mem_16(mem, addr),
        }
    }

    fn set_reg_8(&mut self, reg: Register8, val: u8) {
        let v = val as u16;
        match reg {
            Register8::A => self.AF = (v << 8) | (self.AF & 0xFF),
            Register8::F => self.AF = (self.AF & 0xFF00) | v,
            Register8::B => self.BC = (v << 8) | (self.BC & 0xFF),
            Register8::C => self.BC = (self.BC & 0xFF00) | v,
            Register8::D => self.DE = (v << 8) | (self.DE & 0xFF),
            Register8::E => self.DE = (self.DE & 0xFF00) | v,
            Register8::H => self.HL = (v << 8) | (self.HL & 0xFF),
            Register8::L => self.HL = (self.HL & 0xFF00) | v,
        }
    }

    fn set_reg_16(&mut self, reg: Register16, val: u16) {
        match reg {
            Register16::AF => self.AF = val,
            Register16::BC => self.BC = val,
            Register16::DE => self.DE = val,
            Register16::HL => self.HL = val,
            Register16::SP => self.SP = val,
        }
    }

    fn set_mem_8(&mut self, mem: &mut Memory, addr: u16, val: u8) {
        mem.write_8(addr, val);
    }

    fn set_mem_16(&mut self, mem: &mut Memory, addr: u16, val: u16) {
        mem.write_16(addr, val);
    }

    fn set_val_8(&mut self, mem: &mut Memory, tgt: Operand8, val: u8) {
        match tgt {
            Operand8::Immediate(_) => panic!("Can't write to immediate value"),
            Operand8::Direct(reg) => self.set_reg_8(reg, val),
            Operand8::IndirectAddr(addr) => self.set_mem_8(mem, addr, val),
            Operand8::IndirectReg(reg) => self.set_mem_8(mem, self.get_reg_16(reg), val),
        }
    }

    fn set_val_16(&mut self, mem: &mut Memory, tgt: Operand16, val: u16) {
        match tgt {
            Operand16::Immediate(_) => panic!("Can't write to immediate value"),
            Operand16::Direct(reg) => self.set_reg_16(reg, val),
            Operand16::IndirectAddr(addr) => self.set_mem_16(mem, addr, val),
        }
    }

    fn check_interrupt(&mut self, mem: &mut Memory, interrupt: Interrupt, vec: u16) -> bool {
        if mem.get_intterupt_enable(interrupt) && mem.get_intterupt(interrupt) {
            mem.clear_intterupt(interrupt);
            self.set_mem_16(mem, self.SP - 1, self.PC);
            self.SP -= 2;
            self.enable_interrupts = false;
            self.PC = vec;
            return true;
        }
        return false;
    }

    fn check_interrupts(&mut self, mem: &mut Memory) -> Option<u8> {
        if !self.enable_interrupts {
            return None;
        }

        let mut interrupted = self.check_interrupt(mem, Interrupt::VBlank, VBLANK_VEC);
        
        if !interrupted {
            interrupted = self.check_interrupt(mem, Interrupt::LCD, LCD_VEC);
        }

        if !interrupted {
            interrupted = self.check_interrupt(mem, Interrupt::Timer, TIMER_VEC);
        }
        
        if !interrupted {
            interrupted = self.check_interrupt(mem, Interrupt::Serial, SERIAL_VEC);
        }

        if !interrupted {
            interrupted = self.check_interrupt(mem, Interrupt::Joypad, JOYPAD_VEC);
        }

        if interrupted {
            return Some(20);
        }

        return None;
    }

    // Instructions
    pub fn exec(&mut self, mem: &mut Memory) -> u8 {
        let interrupt_result = self.check_interrupts(mem);
        if let Some(cycles) = interrupt_result {
            // TODO: If in HALT, need 4 more cycles
            return cycles;
        }

        let op = mem.read_8(self.PC);
        debug!(
            "{:#x} {} C:{:?}, Z:{:?}",
            self.PC,
            op,
            ((self.AF >> CARRY_FLAG_SHIFT) & 1) != 0,
            ((self.AF >> ZERO_FLAG_SHIFT) & 1) != 0,
        );

        if op == 0xCB {
            let op = mem.read_8(self.PC + 1);
            let x = unbounded_shr_u8(op & 0b1100_0000, 6u8);
            let y = unbounded_shr_u8(op & 0b0011_1000, 3u8);
            let z = op & 0b0000_0111;

            let src = r_to_source(z);
            if x == 0 {
                match y {
                    0 => self.rotate_left(mem, src, true),
                    1 => self.rotate_right(mem, src, true),
                    2 => self.rotate_left(mem, src, false),
                    3 => self.rotate_right(mem, src, false),
                    4 => self.sla(mem, src),
                    5 => self.sra(mem, src),
                    6 => self.swap(mem, src),
                    7 => self.srl(mem, src),
                    _ => panic!("Unexpected opcode {} after 0xCB prefix", op),
                }

                self.PC += 2;
                // TODO: These aren't correct for everything above.
                match src {
                    Operand8::IndirectReg(_) => return 12,
                    Operand8::Direct(Register8::A) => return 4,
                    _ => return 8,
                }
            } else {
                match x {
                    1 => {
                        debug!("{:#x}: BIT {},{}", self.PC, y, src);
                        self.bit(mem, src, y);
                    }
                    2 => {
                        debug!("{:#x}: RES {},{}", self.PC, y, src);
                        self.unset(mem, src, y)
                    }
                    3 => {
                        debug!("{:#x}: SET {},{}", self.PC, y, src);
                        self.set(mem, src, y)
                    }
                    _ => panic!("Unexpected opcode {} after 0xCB prefix", op),
                }

                self.PC += 2;
                match src {
                    Operand8::IndirectReg(_) => return 16,
                    _ => return 8,
                }
            }
        } else {
            // Op 0x76 (i.e. LD (HL),(HL)) translates to HALT, skip it here.
            if op >= 0x40 && op <= 0x7F && op != 0x76 {
                let tgt = r_to_source(unbounded_shr_u8(op & 0b0011_1000, 3u8));
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: LD {},{}", self.PC, tgt, src);
                self.load8(mem, tgt, src);
                self.PC += 1;

                if let Operand8::IndirectReg(_) = tgt {
                    return 8;
                } else if let Operand8::IndirectReg(_) = src {
                    return 8;
                } else {
                    return 4;
                }
            } else if op >= 0x80 && op <= 0x87 {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: ADD A,{}", self.PC, src);
                self.add8(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0x88 && op <= 0x8F {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: ADC A,{}", self.PC, src);
                self.add_carry8(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0x90 && op <= 0x97 {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: SUB {}", self.PC, src);
                self.sub8(mem, Operand8::Direct(Register8::A), src, false);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0x98 && op <= 0x9F {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: SBC {}", self.PC, src);
                self.sub8(mem, Operand8::Direct(Register8::A), src, true);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0xA0 && op <= 0xA7 {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: AND {}", self.PC, src);
                self.and(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0xA8 && op <= 0xAF {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: XOR {}", self.PC, src);
                self.xor(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0xB0 && op <= 0xB7 {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: OR {}", self.PC, src);
                self.or(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else if op >= 0xB8 && op <= 0xBF {
                let src = r_to_source(op & 0x7);
                debug!("{:#x}: CP {}", self.PC, src);
                self.compare(mem, src);
                self.PC += 1;
                match src {
                    Operand8::IndirectReg(_) => return 8,
                    _ => return 4,
                }
            } else {
                match op {
                    0 => {
                        // NOP
                        self.PC += 1;
                        return 4;
                    }
                    1 => {
                        // LD BC,n16
                        let val = self.read_nn(mem);
                        self.load16(
                            mem,
                            Operand16::Direct(Register16::BC),
                            Operand16::Immediate(val),
                        );
                        self.PC += 3;
                        return 12;
                    }
                    2 => {
                        // LD (BC),A
                        self.load8(
                            mem,
                            Operand8::IndirectReg(Register16::BC),
                            Operand8::Direct(Register8::A),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    3 => {
                        // INC BC
                        self.inc16(mem, Operand16::Direct(Register16::BC));
                        self.PC += 1;
                        return 8;
                    }
                    4 => {
                        // INC B
                        debug!("{:#x}: INC B", self.PC);
                        self.inc8(mem, Operand8::Direct(Register8::B));
                        self.PC += 1;
                        return 4;
                    }
                    5 => {
                        // DEC B
                        debug!("{:#x}: DEC B", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::B));
                        self.PC += 1;
                        return 4;
                    }
                    6 => {
                        // LD B,n8
                        let val = self.read_n(mem);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::B),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    7 => {
                        // RLCA
                        self.rotate_left(mem, Operand8::Direct(Register8::A), true);
                        self.PC += 1;
                        return 4;
                    }
                    8 => {
                        // LD (a16),SP
                        let val = self.read_nn(mem);
                        self.load16(
                            mem,
                            Operand16::IndirectAddr(val),
                            Operand16::Direct(Register16::SP),
                        );
                        self.PC += 3;
                        return 20;
                    }
                    9 => {
                        // Add HL,BC
                        self.add16(
                            mem,
                            Operand16::Direct(Register16::HL),
                            Operand16::Direct(Register16::BC),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    10 => {
                        // LD A,(BC)
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectReg(Register16::BC),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    11 => {
                        // DEC BC
                        debug!("{:#x}: DEC BC", self.PC);
                        self.dec16(mem, Operand16::Direct(Register16::BC));
                        self.PC += 1;
                        return 8;
                    }
                    12 => {
                        // INC C
                        self.inc8(mem, Operand8::Direct(Register8::C));
                        self.PC += 1;
                        return 4;
                    }
                    13 => {
                        // DEC C
                        debug!("{:#x}: DEC C", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::C));
                        self.PC += 1;
                        return 4;
                    }
                    14 => {
                        // LD C,n8
                        let val = self.read_n(mem);
                        debug!("{:#x}: LD C,{:#x}", self.PC, val);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::C),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    15 => {
                        // RRCA
                        self.rotate_right(mem, Operand8::Direct(Register8::A), true);
                        self.PC += 1;
                        return 4;
                    }
                    16 => unimplemented!("STOP not implemeted"),
                    17 => {
                        // LD DE,n16
                        let val = self.read_nn(mem);
                        self.load16(
                            mem,
                            Operand16::Direct(Register16::DE),
                            Operand16::Immediate(val),
                        );
                        self.PC += 3;
                        return 12;
                    }
                    18 => {
                        // LD (DE),A
                        self.load8(
                            mem,
                            Operand8::IndirectReg(Register16::DE),
                            Operand8::Direct(Register8::A),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    19 => {
                        // INC DE
                        self.inc16(mem, Operand16::Direct(Register16::DE));
                        self.PC += 1;
                        return 8;
                    }
                    20 => {
                        // INC D
                        self.inc8(mem, Operand8::Direct(Register8::D));
                        self.PC += 1;
                        return 4;
                    }
                    21 => {
                        // DEC D
                        debug!("{:#x}: DEC D", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::D));
                        self.PC += 1;
                        return 4;
                    }
                    22 => {
                        // LD D,n8
                        let val = self.read_n(mem);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::D),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    23 => {
                        // RLA
                        self.rotate_left(mem, Operand8::Direct(Register8::A), true);
                        self.PC += 1;
                        return 4;
                    }
                    24 => {
                        // JR n8
                        let offset = from_twos_complement(self.read_n(mem)) as i32;
                        debug!("{:#x}: JR {}", self.PC, offset);
                        self.PC += 2;
                        self.PC = (self.PC as i32 + offset) as u16;
                        return 12;
                    }
                    25 => {
                        // ADD HL,DE
                        self.add16(
                            mem,
                            Operand16::Direct(Register16::HL),
                            Operand16::Direct(Register16::DE),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    26 => {
                        // LD A,(DE)
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectReg(Register16::DE),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    27 => {
                        // DEC DE
                        debug!("{:#x}: DEC DE", self.PC);
                        self.dec16(mem, Operand16::Direct(Register16::DE));
                        self.PC += 1;
                        return 8;
                    }
                    28 => {
                        // INC E
                        self.inc8(mem, Operand8::Direct(Register8::E));
                        self.PC += 1;
                        return 4;
                    }
                    29 => {
                        // DEC E
                        debug!("{:#x}: DEC E", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::E));
                        self.PC += 1;
                        return 4;
                    }
                    30 => {
                        // LD E,n8
                        let val = self.read_n(mem);
                        debug!("{:#x}: LD E,{:#x}", self.PC, val);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::E),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    31 => {
                        // RRA
                        self.rotate_right(mem, Operand8::Direct(Register8::A), true);
                        self.PC += 1;
                        return 4;
                    }
                    32 => {
                        // JR NZ,n8
                        let offset = from_twos_complement(self.read_n(mem));
                        debug!("{:#x}: JR NZ,{}", self.PC, offset);
                        return self.conditional_relative_jump(Condition::NZ, offset);
                    }
                    33 => {
                        // LD HL,n16
                        let val = self.read_nn(mem);
                        debug!("{:#x}: LD HL,{:#x}", self.PC, val);
                        self.load16(
                            mem,
                            Operand16::Direct(Register16::HL),
                            Operand16::Immediate(val),
                        );
                        self.PC += 3;
                        return 12;
                    }
                    34 => {
                        // LD (HL+),A
                        self.load8(
                            mem,
                            Operand8::IndirectReg(Register16::HL),
                            Operand8::Direct(Register8::A),
                        );
                        self.HL += 1;
                        self.PC += 1;
                        return 8;
                    }
                    35 => {
                        // INC HL
                        self.inc16(mem, Operand16::Direct(Register16::HL));
                        self.PC += 1;
                        return 8;
                    }
                    36 => {
                        // INC H
                        self.inc8(mem, Operand8::Direct(Register8::H));
                        self.PC += 1;
                        return 4;
                    }
                    37 => {
                        // DEC H
                        debug!("{:#x}: DEC H", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::H));
                        self.PC += 1;
                        return 4;
                    }
                    38 => {
                        // LD H,n8
                        let val = self.read_n(mem);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::H),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    0x27 => {
                        // DAA
                        debug!("{:#x}: DAA", self.PC);
                        self.PC += 1;
                        return self.daa();
                    }
                    0x28 => {
                        // JR Z,n8
                        let offset = from_twos_complement(self.read_n(mem));
                        return self.conditional_relative_jump(Condition::Z, offset);
                    }
                    41 => {
                        // ADD HL,HL
                        self.add16(
                            mem,
                            Operand16::Direct(Register16::HL),
                            Operand16::Direct(Register16::HL),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    42 => {
                        // LD A,(HL+)
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectReg(Register16::HL),
                        );
                        self.HL += 1;
                        self.PC += 1;
                        return 8;
                    }
                    43 => {
                        // DEC HL
                        debug!("{:#x}: DEC HL", self.PC);
                        self.dec16(mem, Operand16::Direct(Register16::HL));
                        self.PC += 1;
                        return 8;
                    }
                    44 => {
                        // INC L
                        self.inc8(mem, Operand8::Direct(Register8::L));
                        self.PC += 1;
                        return 4;
                    }
                    45 => {
                        // DEC L
                        debug!("{:#x}: DEC L", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::L));
                        self.PC += 1;
                        return 4;
                    }
                    46 => {
                        // LD L,n8
                        let val = self.read_n(mem);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::L),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    47 => {
                        let val = self.get_reg_8(Register8::A);
                        self.set_reg_8(Register8::A, !val);
                        self.PC += 1;
                        return 4;
                    }
                    48 => {
                        // JR NC,r8
                        let offset = from_twos_complement(self.read_n(mem));
                        debug!("{:#x}: JR NC,{:#x}", self.PC, offset);
                        return self.conditional_relative_jump(Condition::NC, offset);
                    }
                    49 => {
                        // LD SP,n16
                        let val = self.read_nn(mem);
                        debug!("{:#x} LD SP,{:#x}", self.PC, val);
                        self.load16(
                            mem,
                            Operand16::Direct(Register16::SP),
                            Operand16::Immediate(val),
                        );
                        self.PC += 3;
                        return 12;
                    }
                    50 => {
                        // LD (HL-),A
                        debug!("{:#x}: LD (HL-),A", self.PC);
                        self.load8(
                            mem,
                            Operand8::IndirectReg(Register16::HL),
                            Operand8::Direct(Register8::A),
                        );
                        self.HL -= 1;
                        self.PC += 1;
                        return 8;
                    }
                    51 => {
                        // INC SP
                        self.inc16(mem, Operand16::Direct(Register16::SP));
                        self.PC += 1;
                        return 8;
                    }
                    52 => {
                        // INC (HL)
                        self.inc8(mem, Operand8::IndirectReg(Register16::HL));
                        self.PC += 1;
                        return 8;
                    }
                    53 => {
                        // DEC (HL)
                        debug!("{:#x}: DEC (HL)", self.PC);
                        self.dec8(mem, Operand8::IndirectReg(Register16::HL));
                        self.PC += 1;
                        return 8;
                    }
                    54 => {
                        // LD (HL),n8
                        let val = self.read_n(mem);
                        self.load8(
                            mem,
                            Operand8::IndirectReg(Register16::HL),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    55 => {
                        // SCF
                        self.set_flags(self.get_zero_flag(), false, false, true);
                        self.PC += 1;
                        return 4;
                    }
                    56 => {
                        // JR C,n8
                        let offset = from_twos_complement(self.read_n(mem));
                        debug!("{:#x}: JR C,{:#x}", self.PC, offset);
                        return self.conditional_relative_jump(Condition::C, offset);
                    }
                    57 => {
                        // ADD HL,SP
                        self.add16(
                            mem,
                            Operand16::Direct(Register16::HL),
                            Operand16::Direct(Register16::SP),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    58 => {
                        // LD A,(HL-)
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectReg(Register16::HL),
                        );
                        self.HL -= 0;
                        self.PC += 1;
                        return 8;
                    }
                    59 => {
                        // DEC SP
                        debug!("{:#x}: DEC SP", self.PC);
                        self.dec16(mem, Operand16::Direct(Register16::SP));
                        self.PC += 1;
                        return 8;
                    }
                    60 => {
                        // INC A
                        self.inc8(mem, Operand8::Direct(Register8::A));
                        self.PC += 1;
                        return 4;
                    }
                    61 => {
                        // DEC A
                        debug!("{:#x}: DEC A", self.PC);
                        self.dec8(mem, Operand8::Direct(Register8::A));
                        self.PC += 1;
                        return 4;
                    }
                    62 => {
                        // LD A,n8
                        let val = self.read_n(mem);
                        debug!("{:#x}: LD A,{:#x}", self.PC, val);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::Immediate(val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    63 => {
                        // CCF
                        self.set_flags(self.get_zero_flag(), false, false, false);
                        self.PC += 1;
                        return 4;
                    }
                    118 => {
                        // HALT
                        unimplemented!("HALT not implemented")
                    }
                    192 => {
                        // RET NZ
                        return self.conditional_ret(mem, Condition::NZ);
                    }
                    193 => {
                        // POP BC
                        self.pop(mem, Register16::BC);
                        self.PC += 1;
                        return 12;
                    }
                    194 => {
                        // JP NZ,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_jump(Condition::NZ, addr);
                    }
                    195 => {
                        // JP n16
                        self.PC = self.read_nn(mem);
                        return 16;
                    }
                    196 => {
                        // CALL NZ,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_call(mem, Condition::NZ, addr);
                    }
                    197 => {
                        // PUSH BC
                        self.push(mem, Register16::BC);
                        self.PC += 1;
                        return 16;
                    }
                    198 => {
                        // ADD A,n8
                        let val = self.read_n(mem);
                        self.add8(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    199 => {
                        // RST 00H
                        debug!("{:#x}: RST 00H", self.PC);
                        return self.rst (mem, 0x0000);
                    }
                    200 => {
                        // RET Z
                        return self.conditional_ret(mem, Condition::Z);
                    }
                    201 => {
                        // RET
                        return self.ret(mem);
                    }
                    202 => {
                        // JP Z,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_jump(Condition::Z, addr);
                    }
                    0xCB => panic!("Unexpected 0xCB prefix"),
                    0xCC => {
                        // CALL Z,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_call(mem, Condition::Z, addr);
                    }
                    0xCD => {
                        // CALL n16
                        let addr = self.read_nn(mem);
                        return self.call(mem, addr);
                    }
                    0xCE => {
                        // ADC n8
                        let val = self.read_n(mem);
                        self.add_carry8(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    0xCF => {
                        // RST 08H
                        debug!("{:#x}: RST 08H", self.PC);
                        return self.rst(mem, 0x0008);
                    }
                    0xD0 => {
                        // RET NC
                        return self.conditional_ret(mem, Condition::NC);
                    }
                    0xD1 => {
                        // POP DE
                        self.pop(mem, Register16::DE);
                        self.PC += 1;
                        return 12;
                    }
                    0xD2 => {
                        // JP NC,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_jump(Condition::NC, addr);
                    }
                    0xD4 => {
                        // CALL NC,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_call(mem, Condition::NC, addr);
                    }
                    0xD5 => {
                        // PUSH DE
                        self.push(mem, Register16::DE);
                        self.PC += 1;
                        return 16;
                    }
                    0xD6 => {
                        // SUB n8
                        let val = self.read_n(mem);
                        self.sub8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::Immediate(val),
                            false,
                        );
                        self.PC += 2;
                        return 8;
                    }
                    0xD7 => {
                        // RST 10H
                        debug!("{:#x}: RST 10H", self.PC);
                        return self.rst(mem, 0x0010);
                    }
                    0xD8 => {
                        // RET C
                        return self.conditional_ret(mem, Condition::C);
                    }
                    0xD9 => {
                        // RETI
                        self.enable_interrupts = true;
                        return self.ret(mem);
                    }
                    0xDA => {
                        // JP C,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_jump(Condition::C, addr);
                    }
                    0xDC => {
                        // CALL C,n16
                        let addr = self.read_nn(mem);
                        return self.conditional_call(mem, Condition::C, addr);
                    }
                    0xDE => {
                        // SBC n8
                        let val = self.read_n(mem);
                        self.sub8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::Immediate(val),
                            true,
                        );
                        self.PC += 2;
                        return 8;
                    }
                    0xDF => {
                        // RST 18H
                        debug!("{:#x}: RST 18H", self.PC);
                        return self.rst(mem, 0x0018);
                    }
                    0xE0 => {
                        // LDH (n8),A
                        let val = self.read_n(mem) as u16;
                        debug!("{:#x}: LDH ({:#x}),A", self.PC, val);
                        self.load8(
                            mem,
                            Operand8::IndirectAddr(0xFF00 + val),
                            Operand8::Direct(Register8::A),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    0xE1 => {
                        // POP HL
                        self.pop(mem, Register16::HL);
                        self.PC += 1;
                        return 12;
                    }
                    0xE2 => {
                        // LD (C),A
                        let val = self.get_reg_8(Register8::C) as u16;
                        self.load8(
                            mem,
                            Operand8::IndirectAddr(0xFF00 + val),
                            Operand8::Direct(Register8::A),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    0xE5 => {
                        // PUSH HL
                        self.push(mem, Register16::HL);
                        self.PC += 1;
                        return 16;
                    }
                    0xE6 => {
                        // AND n8
                        let val = self.read_n(mem);
                        self.and(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    0xE7 => {
                        // RST 20H
                        debug!("{:#x}: RST 20H", self.PC);
                        return self.call(mem, 0x0020);
                    }
                    0xE8 => {
                        // ADD SP, n8
                        let val = self.read_n(mem) as u16;
                        self.add16(
                            mem,
                            Operand16::Direct(Register16::SP),
                            Operand16::Immediate(val),
                        );
                        self.PC += 2;
                        return 16;
                    }
                    0xE9 => {
                        // JP (HL)
                        self.PC = self.get_reg_16(Register16::HL);
                        return 4;
                    }
                    0xEA => {
                        // LD (n16),A
                        let addr = self.read_nn(mem);
                        debug!("{:#x}: ({:#x}),A", self.PC, addr);
                        self.load8(
                            mem,
                            Operand8::IndirectAddr(addr),
                            Operand8::Direct(Register8::A),
                        );
                        self.PC += 3;
                        return 8;
                    }
                    0xEE => {
                        // XOR n8
                        let val = self.read_n(mem);
                        self.xor(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    0xEF => {
                        // RST 28H
                        debug!("{:#x}: RST 28H", self.PC);
                        return self.rst(mem, 0x0028);
                    }
                    0xF0 => {
                        // LDH A,(n8)
                        let val = self.read_n(mem) as u16;
                        debug!("{:#x}: LDH A,({:#x})", self.PC, val);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectAddr(0xFF00 + val),
                        );
                        self.PC += 2;
                        return 8;
                    }
                    0xF1 => {
                        // POP AF
                        self.pop(mem, Register16::AF);
                        self.PC += 1;
                        return 12;
                    }
                    0xF2 => {
                        // LDH A,(C)
                        let val = self.get_reg_8(Register8::C) as u16;
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectAddr(0xFF00 + val),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    0xF3 => { // DI
                        debug!("{:#x}: EI", self.PC);
                        self.enable_interrupts = false;
                        self.PC += 1;
                        return 4;
                    }
                    0xF5 => {
                        // PUSH AF
                        self.push(mem, Register16::AF);
                        self.PC += 1;
                        return 16;
                    }
                    0xF6 => {
                        // OR n8
                        let val = self.read_n(mem);
                        self.or(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    0xF7 => {
                        // RST 30H
                        debug!("{:#x}: RST 30H", self.PC);
                        return self.rst(mem, 0x0030);
                    }
                    0xF8 => {
                        // LD HL,SP+n8
                        todo!("LD HL,SP+n8 not implemented")
                    }
                    0xF9 => {
                        // LD SP,HL
                        self.load16(
                            mem,
                            Operand16::Direct(Register16::SP),
                            Operand16::Direct(Register16::HL),
                        );
                        self.PC += 1;
                        return 8;
                    }
                    0xFA => {
                        // LD A,(n16)
                        let addr = self.read_nn(mem);
                        self.load8(
                            mem,
                            Operand8::Direct(Register8::A),
                            Operand8::IndirectAddr(addr),
                        );
                        self.PC += 3;
                        return 8;
                    }
                    0xFB => { // EI
                        debug!("{:#x}: EI", self.PC);
                        self.enable_interrupts = true;
                        self.PC += 1;
                        return 4;
                    }
                    0xFE => {
                        // CP n8
                        let val = self.read_n(mem);
                        debug!("{:#x}: CP {:#x}", self.PC, val);
                        self.compare(mem, Operand8::Immediate(val));
                        self.PC += 2;
                        return 8;
                    }
                    0xFF => {
                        // RST 38H
                        debug!("{:#x}: RST 38H", self.PC);
                        return self.rst(mem, 0x0038);
                    }
                    _ => panic!("Unexpected instruction {:#x}", op),
                }
            }
        }
    }

    fn inc8(&mut self, mem: &mut Memory, tgt: Operand8) {
        let val = self.get_val_8(mem, tgt);
        self.set_val_8(mem, tgt, val + 1);

        // TODO: Flags
    }

    fn dec8(&mut self, mem: &mut Memory, tgt: Operand8) {
        let val = match self.get_val_8(mem, tgt) {
            0 => 0xFF,
            v => v - 1,
        };
        self.set_val_8(mem, tgt, val);

        // Half-carry - did we borrow from bit 4?
        let half_carry = val == 0b1111;

        self.set_flags(val == 0, true, half_carry, self.get_carry_flag());
    }

    fn inc16(&mut self, mem: &mut Memory, tgt: Operand16) {
        let val = self.get_val_16(mem, tgt);
        self.set_val_16(mem, tgt, val + 1);
    }

    fn dec16(&mut self, mem: &mut Memory, tgt: Operand16) {
        let val = self.get_val_16(mem, tgt);
        self.set_val_16(mem, tgt, val - 1);
    }

    fn load8(&mut self, mem: &mut Memory, tgt: Operand8, src: Operand8) {
        let val = self.get_val_8(mem, src);
        self.set_val_8(mem, tgt, val);
    }

    fn load16(&mut self, mem: &mut Memory, tgt: Operand16, src: Operand16) {
        let val = self.get_val_16(mem, src);
        self.set_val_16(mem, tgt, val);
    }

    fn add8(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let acc = self.get_reg_8(Register8::A);
        let wide: u16 = val as u16 + acc as u16;
        let res = (wide & 0xFF) as u8;

        // Half overflow (bit 3)
        let h = ((val & 0xF) + (acc & 0xF)) > 0xF;
        let c = wide > 0xFF;
        let n = false;
        let z = res == 0;
        self.set_af(res, z, n, h, c);
    }

    fn add_carry8(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let carry = ((self.AF >> 4) & 0x1) as u8;
        let acc = self.get_reg_8(Register8::A);
        let wide: u16 = val as u16 + acc as u16 + carry as u16;
        let res = (wide & 0xFF) as u8;

        // Half overflow (bit 3)
        let h = ((val & 0xF) + (acc & 0xF) + carry) > 0xF;
        let c = wide > 0xFF;
        let n = false;
        let z = res == 0;
        self.set_af(res, z, n, h, c);
    }

    fn add16(&mut self, mem: &mut Memory, tgt: Operand16, src: Operand16) {
        let val = self.get_val_16(mem, src);
        let cur = self.get_val_16(mem, tgt);
        let wide: u32 = val as u32 + cur as u32;
        let res = (wide & 0xFFFF) as u16;

        // Half overflow (bit 11)
        let h = ((val & 0xFFF) + (cur & 0xFFF)) > 0xFFF;
        let c = wide > 0xFFFF;
        let n = false;
        let z = ((self.AF >> ZERO_FLAG_SHIFT) & 0x1) != 0;
        self.set_val_16(mem, tgt, res);
        self.set_flags(z, n, h, c);
    }

    fn sub8(&mut self, mem: &mut Memory, tgt: Operand8, sub: Operand8, with_carry: bool) {
        let c: u8 = if with_carry {
            ((self.AF >> CARRY_FLAG_SHIFT) & 0x1) as u8
        } else {
            0
        };
        let tgt_val = self.get_val_8(mem, tgt);
        let sub_val = self.get_val_8(mem, sub) + c;

        let res = tgt_val - sub_val;
        self.set_val_8(mem, tgt, res);

        let h = (sub_val & 0xFF) > (tgt_val & 0xFF);
        self.set_flags(res == 0, true, h, sub_val > tgt_val);
    }

    fn bit(&mut self, mem: &Memory, tgt: Operand8, mask: u8) {
        let val = self.get_val_8(mem, tgt);
        let res = val & (1 << mask) == 0;
        self.set_flags(res, false, true, self.get_carry_flag());
    }

    fn set(&mut self, mem: &mut Memory, tgt: Operand8, mask: u8) {
        let val = self.get_val_8(mem, tgt);
        let res = val | (1 << mask);
        self.set_val_8(mem, tgt, res);
    }

    fn unset(&mut self, mem: &mut Memory, tgt: Operand8, mask: u8) {
        let val = self.get_val_8(mem, tgt);
        let res = val & !(1 << mask);
        self.set_val_8(mem, tgt, res);
    }

    fn and(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let acc = self.get_reg_8(Register8::A);
        let res = acc & val;
        self.set_af(res, res == 0, false, true, false);
    }

    fn or(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let acc = self.get_reg_8(Register8::A);
        let res = acc | val;
        self.set_af(res, res == 0, false, false, false);
    }

    fn xor(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let acc = self.get_reg_8(Register8::A);
        let res = acc ^ val;
        self.set_af(res, res == 0, false, false, false);
    }

    fn compare(&mut self, mem: &Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let acc = self.get_reg_8(Register8::A);

        let half = val & 0b1111 > acc & 0b1111;
        self.set_flags(val == acc, true, half, val > acc);
    }

    fn conditional_jump(&mut self, cond: Condition, addr: u16) -> u8 {
        self.PC += 3;
        if self.eval_condition(cond) {
            self.PC = addr;
            return 16;
        } else {
            return 12;
        }
    }

    fn conditional_relative_jump(&mut self, cond: Condition, offset: i8) -> u8 {
        self.PC += 2;
        if self.eval_condition(cond) {
            self.PC = (self.PC as i32 + offset as i32) as u16;
            return 12;
        } else {
            return 8;
        }
    }

    fn rst(&mut self, mem: &mut Memory, addr: u16) -> u8 {
        let ret = self.PC + 1;
        self.set_mem_16(mem, self.SP - 1, ret);
        self.SP -= 2;
        self.PC = addr;
        return 16;
    }

    fn call(&mut self, mem: &mut Memory, addr: u16) -> u8 {
        let ret = self.PC + 3;
        self.set_mem_16(mem, self.SP - 1, ret);
        self.SP -= 2;
        self.PC = addr;
        return 24;
    }

    fn conditional_call(&mut self, mem: &mut Memory, cond: Condition, addr: u16) -> u8 {
        if self.eval_condition(cond) {
            return self.call(mem, addr);
        } else {
            self.PC += 3;
            return 12;
        }
    }

    fn ret(&mut self, mem: &Memory) -> u8 {
        let addr = self.get_mem_16(mem, self.SP + 1);
        self.SP += 2;
        self.PC = addr;
        return 16;
    }

    fn conditional_ret(&mut self, mem: &Memory, cond: Condition) -> u8 {
        if self.eval_condition(cond) {
            return self.ret(mem) + 4;
        } else {
            self.PC += 1;
            return 8;
        }
    }

    fn pop(&mut self, mem: &Memory, tgt: Register16) {
        let val = self.get_mem_16(mem, self.SP + 1);
        self.set_reg_16(tgt, val);
        self.SP += 2;
    }

    fn push(&mut self, mem: &mut Memory, src: Register16) {
        let val = self.get_reg_16(src);
        self.set_mem_16(mem, self.SP - 1, val);
        self.SP -= 2;
    }

    fn rotate_left(&mut self, mem: &mut Memory, src: Operand8, through_carry: bool) {
        let val = self.get_val_8(mem, src);

        let old_carry = (self.AF >> CARRY_FLAG_SHIFT & 0x1) as u8;
        let old_b7 = (val & 0b1000_0000) >> 7;
        let new_b0 = if through_carry { old_carry } else { old_b7 };
        let res = ((val & 0b0111_1111) << 1) + new_b0;
        self.set_val_8(mem, src, res);

        let zero = if let Operand8::Direct(Register8::A) = src {
            // RLA/RLCA clear the zero flag.
            false
        } else {
            res == 0
        };
        self.set_flags(zero, false, false, old_b7 == 1);
    }

    fn rotate_right(&mut self, mem: &mut Memory, src: Operand8, through_carry: bool) {
        let val = self.get_val_8(mem, src);

        let old_carry = (self.AF >> CARRY_FLAG_SHIFT & 0x1) as u8;
        let old_b0 = val & 0x1;
        let new_b7 = if through_carry { old_carry } else { old_b0 };
        let res = (new_b7 << 7) + ((val & 0b1111_1110) >> 1);
        self.set_val_8(mem, src, res);

        let zero = if let Operand8::Direct(Register8::A) = src {
            // RRA/RRCA clear the zero flag.
            false
        } else {
            res == 0
        };
        self.set_flags(zero, false, false, old_b0 == 1);
    }

    fn swap(&mut self, mem: &mut Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let upper = val & 0xF0;
        let lower = val & 0x0F;
        let result = (lower << 4) | (upper >> 4);
        self.set_val_8(mem, src, result);
        self.set_flags(result == 0, false, false, false);
    }

    fn sla(&mut self, mem: &mut Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let carry = unbounded_shr_u8(val, 7u8) > 0;
        let result = (val & 0b0111_1111) << 1;
        self.set_val_8(mem, src, result);
        self.set_flags(result == 0, false, false, carry);
    }

    fn sra(&mut self, mem: &mut Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let carry = (val & 0x1) > 0;
        let result = (val & 0b1000_0000) | (val >> 1);
        self.set_val_8(mem, src, result);
        self.set_flags(result == 0, false, false, carry);
    }

    fn srl(&mut self, mem: &mut Memory, src: Operand8) {
        let val = self.get_val_8(mem, src);
        let carry = (val & 0x1) > 0;
        let result = val >> 1;
        self.set_val_8(mem, src, result);
        self.set_flags(result == 0, false, false, carry);
    }

    fn daa(&mut self) -> u8 {
        let mut adj = 0;
        let acc = self.get_reg_8(Register8::A);
        let result = if self.get_neg_flag() {
            if self.get_half_carry_flag() {
                adj += 0x6;
            }
            if self.get_carry_flag() {
                adj += 0x60;
            }
            acc - adj
        } else {
            if self.get_half_carry_flag() || (acc & 0xF) > 0x9 {
                adj += 0x6;
            }
            if self.get_carry_flag() || acc > 0x99 {
                adj += 0x60;
            }
            acc + adj
        };

        self.set_reg_8(Register8::A, result);

        let carry = (adj & 0x60) > 0;
        self.set_flags(result == 0, self.get_neg_flag(), false, carry);

        return 4;
    }
}
