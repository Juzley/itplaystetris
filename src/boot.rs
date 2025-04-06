use std::fs::File;
use std::io::Read;

pub struct Boot {
    rom: [u8; 256],
}

impl Boot {
    pub fn load() -> Self {
        let mut boot = Self { rom: [0; 256] };

        let mut f = File::open("dmg_boot.bin").unwrap();
        f.read(&mut boot.rom).unwrap();

        return boot;
    }

    pub fn read_mem_8(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }
}
