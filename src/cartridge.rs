use log::debug;
use std::fs::read;

const RAM_SIZE: usize = 8 * 1024;

pub const CARTRIDGE_ROM_UPPER: u16 = 0x8000;
pub const CARTRIDGE_RAM_LOWER: u16 = 0xA000;
pub const CARTRIDGE_RAM_UPPER: u16 = 0xC000;

pub struct Cartridge {
    rom: Vec<u8>,
    ram: [u8; RAM_SIZE],
}

impl Cartridge {
    pub fn load() -> Self {
        Self {
            rom: read("cartridge.gb").unwrap(),
            ram: [0; RAM_SIZE],
        }
    }

    pub fn read_mem_8(&self, addr: u16) -> u8 {
        if addr < CARTRIDGE_ROM_UPPER {
            return self.rom[addr as usize];
        } else if addr >= CARTRIDGE_RAM_LOWER && addr < CARTRIDGE_RAM_UPPER {
            return self.ram[(addr - CARTRIDGE_RAM_LOWER) as usize];
        } else {
            panic!("Out of bounds cartridge read: {:#x}", addr);
        }
    }

    pub fn write_mem_8(&mut self, addr: u16, val: u8) {
        if addr < CARTRIDGE_ROM_UPPER {
            debug!("MBC not supported, ignoring catridge ROM write");
        } else if addr >= CARTRIDGE_RAM_LOWER && addr < CARTRIDGE_RAM_UPPER {
            self.ram[(addr - CARTRIDGE_RAM_LOWER) as usize] = val;
        } else {
            panic!("Out of bounds cartridge write: {:#x}", addr);
        }
    }
}
