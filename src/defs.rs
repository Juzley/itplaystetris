#![allow(non_snake_case)]

use std::fmt;

#[derive(Clone, Copy, Debug)]
pub enum Interrupt {
    Joypad = 4,
    Serial = 3,
    Timer = 2,
    LCD = 1,
    VBlank = 0,
}

#[derive(Clone, Copy, Debug)]
pub enum Condition {
    NZ = 0,
    Z = 1,
    NC = 2,
    C = 3,
}

#[derive(Clone, Copy, Debug)]
pub enum Register16 {
    BC,
    DE,
    HL,
    SP,
    AF,
}

impl fmt::Display for Register16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Register16::BC => write!(f, "BC"),
            Register16::DE => write!(f, "DE"),
            Register16::HL => write!(f, "HL"),
            Register16::SP => write!(f, "SP"),
            Register16::AF => write!(f, "AF"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Register8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl fmt::Display for Register8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Register8::A => write!(f, "A"),
            Register8::F => write!(f, "F"),
            Register8::B => write!(f, "B"),
            Register8::C => write!(f, "C"),
            Register8::D => write!(f, "D"),
            Register8::E => write!(f, "E"),
            Register8::H => write!(f, "H"),
            Register8::L => write!(f, "L"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Operand8 {
    Immediate(u8),
    IndirectAddr(u16),
    IndirectReg(Register16),
    Direct(Register8),
}

impl fmt::Display for Operand8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand8::Immediate(v) => write!(f, "{}", v),
            Operand8::IndirectAddr(a) => write!(f, "({})", a),
            Operand8::IndirectReg(r) => write!(f, "({})", r),
            Operand8::Direct(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Operand16 {
    Immediate(u16),
    IndirectAddr(u16),
    Direct(Register16),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Button {
    Start = 0x8,
    Select = 0x4,
    B = 0x2,
    A = 0x1,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    Down = 0x8,
    Up = 0x4,
    Left = 0x2,
    Right = 0x1,
}
