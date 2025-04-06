use log::debug;

use crate::boot::Boot;
use crate::cartridge::Cartridge;
use crate::defs::{Button, Direction, Interrupt};
use crate::display::{SpriteSize, TileArea, TileView};

const RAM0_SIZE: usize = 8192;
const RAM1_SIZE: usize = 127;

const VRAM_START: u16 = 0x8000;
const VRAM_SIZE: usize = 10 * 1024;
const SPRITE_ATTRIBUTES_START: u16 = 0xFE00;
const SPRITE_ATTRIBUTES_SIZE: usize = 160;

#[derive(Clone, Copy, Eq, PartialEq)]
enum InputReadMode {
    Direction,
    Buttons,
}

pub struct Memory {
    boot: Boot,
    cartridge: Cartridge,
    use_boot: bool,
    ram0: [u8; RAM0_SIZE],
    ram1: [u8; RAM1_SIZE],
    vram: [u8; VRAM_SIZE],
    sprite_attributes: [u8; SPRITE_ATTRIBUTES_SIZE],

    // LCD
    lcdc: u8,
    stat: u8,
    pub ly: u8,
    pub scx: u8,
    pub scy: u8,
    pub bg_palette: u8,
    pub sprite_palette_1: u8,
    pub sprite_palette_2: u8,

    // Interrupts
    pub interrupt_flags: u8,
    pub interrupt_enables: u8,

    // Timer
    pub timer_tac: u8,
    pub timer_tima: u8,
    pub timer_tma: u8,

    pub div: u8,

    // Input
    button_mask: u8,
    direction_mask: u8,
    input_read_mode: InputReadMode,
}

impl Default for Memory {
    fn default() -> Memory {
        Memory {
            boot: Boot::load(),
            cartridge: Cartridge::load(),
            use_boot: true,
            ram0: [0; RAM0_SIZE],
            ram1: [0; RAM1_SIZE],
            vram: [0; VRAM_SIZE],
            sprite_attributes: [0; SPRITE_ATTRIBUTES_SIZE],

            ly: 0,
            scx: 0,
            scy: 0,
            lcdc: 0,
            stat: 0,
            bg_palette: 0,
            sprite_palette_1: 0,
            sprite_palette_2: 0,

            interrupt_flags: 0,
            interrupt_enables: 0,

            timer_tac: 0,
            timer_tima: 0,
            timer_tma: 0,

            div: 0,

            button_mask: 0b1111,
            direction_mask: 0b1111,
            input_read_mode: InputReadMode::Buttons,
        }
    }
}

impl Memory {
    pub fn set_button(&mut self, button: Button) {
        self.button_mask &= !(button as u8);
    }

    pub fn clear_button(&mut self, button: Button) {
        self.button_mask |= button as u8;
    }

    pub fn set_direction(&mut self, dir: Direction) {
        self.direction_mask &= !(dir as u8);
    }

    pub fn clear_direction(&mut self, dir: Direction) {
        self.direction_mask |= dir as u8;
    }

    pub fn lcdc_bg_enable(&self) -> bool {
        (self.lcdc & 0x1) > 0
    }

    pub fn lcdc_sprite_enable(&self) -> bool {
        (self.lcdc & 0x2) > 0
    }

    pub fn lcdc_sprite_size(&self) -> SpriteSize {
        if (self.lcdc & 0x4) > 0 {
            SpriteSize::Large
        } else {
            SpriteSize::Small
        }
    }

    pub fn lcdc_bg_tile_view(&self) -> TileView {
        if (self.lcdc & 0x8) > 0 {
            TileView::View2
        } else {
            TileView::View1
        }
    }

    pub fn lcdc_bg_tile_area(&self) -> TileArea {
        if (self.lcdc & 0x10) > 0 {
            TileArea::Area1
        } else {
            TileArea::Area2
        }
    }

    pub fn lcdc_window_enable(&self) -> bool {
        (self.lcdc & 0x20) > 0
    }

    pub fn lcdc_window_tile_view(&self) -> TileView {
        if (self.lcdc & 0x40) > 0 {
            TileView::View2
        } else {
            TileView::View1
        }
    }

    pub fn lcdc_enable(&self) -> bool {
        (self.lcdc & 0x80) > 0
    }

    pub fn set_intterupt(&mut self, int: Interrupt) {
        self.interrupt_flags |= 1 << (int as u8);
    }

    pub fn clear_intterupt(&mut self, int: Interrupt) {
        self.interrupt_flags &= !(1 << (int as u8));
    }

    pub fn get_intterupt(&mut self, int: Interrupt) -> bool {
        (self.interrupt_flags >> (int as u8) & 0x1) > 0
    }

    pub fn get_intterupt_enable(&mut self, int: Interrupt) -> bool {
        (self.interrupt_enables >> (int as u8) & 0x1) > 0
    }

    pub fn read_8(&self, addr: u16) -> u8 {
        if addr < 0x4000 {
            // 16kb Catridge rom
            if addr < 0x100 && self.use_boot {
                return self.boot.read_mem_8(addr);
            }
            return self.cartridge.read_mem_8(addr);
        } else if addr < 0x8000 {
            // 16kb Switchable catridge rom
            return self.cartridge.read_mem_8(addr);
        } else if addr >= 0x8000 && addr < 0xA000 {
            // 8kb VRAM
            let offset = (addr - VRAM_START) as usize;
            return self.vram[offset];
        } else if addr < 0xC000 {
            // 8kb Switchable catridge ram
            return self.cartridge.read_mem_8(addr);
        } else if addr < 0xE000 {
            // 8kb main ram
            let offset = (addr - 0xC000) as usize;
            return self.ram0[offset];
        } else if addr < 0xFE00 {
            // 8kb Echo of main ram
            // Translate back to the actual ram
            return self.read_8(addr - 0x2000);
        } else if addr < 0xFEA0 {
            // 160b Sprite Attribute Memory
            let offset = (addr - SPRITE_ATTRIBUTES_START) as usize;
            return self.sprite_attributes[offset];
        } else if addr < 0xFF00 {
            // FEA0-FEFF - Unmapped, reads give 0xFF
            return 0xFF;
        } else if addr < 0xFF80 {
            // FF00-FF7F - I/O Ports
            match addr {
                0xFF00 => {
                    if self.input_read_mode == InputReadMode::Buttons {
                        return (0b1101 << 4) | self.button_mask;
                    } else {
                        return (0b1110 << 4) | self.direction_mask;
                    }
                }
                0xFF04 => return self.div,
                0xFF05 => return self.timer_tima,
                0xFF06 => return self.timer_tma,
                0xFF07 => return self.timer_tac,
                0xFF0F => return self.interrupt_flags,
                0xFF40 => return self.lcdc,
                0xFF41 => return self.stat,
                0xFF42 => return self.scy,
                0xFF43 => return self.scx,
                0xFF44 => return self.ly,
                0xFF47 => return self.bg_palette,
                0xFF48 => return self.sprite_palette_1,
                0xFF49 => return self.sprite_palette_2,
                0xFF50 => return self.use_boot.into(),
                
                _ => unimplemented!("Read from {:#x} not implemented", addr),
            }
        } else if addr < 0xFFFF {
            // FF80-FFFE - High RAM (HRAM)
            let offset = (addr - 0xFF80) as usize;
            return self.ram1[offset];
        } else {
            // 0xFFFF: Interrupt enabled register
            return self.interrupt_enables;
        }
    }

    pub fn read_16(&self, addr: u16) -> u16 {
        let low = self.read_8(addr) as u16;
        let high = self.read_8(addr + 1) as u16;

        (high << 8) + low
    }

    pub fn write_8(&mut self, addr: u16, val: u8) {
        if addr < 0x4000 {
            // 16kb Catridge rom
            self.cartridge.write_mem_8(addr, val);
        } else if addr < 0x8000 {
            // 16kb Switchable catridge rom
            panic!("Attempted write to cartridge ROM");
        } else if addr < 0xA000 {
            // 8kb VRAM
            let offset = (addr - VRAM_START) as usize;
            self.vram[offset] = val;
        } else if addr < 0xC000 {
            // 8kb Switchable catridge ram
            self.cartridge.write_mem_8(addr, val);
        } else if addr < 0xE000 {
            // 8kb Gameboy ram
            let offset = (addr - 0xC000) as usize;
            self.ram0[offset] = val;
        } else if addr < 0xFE00 {
            // 8kb Echo of gameboy ram
            // Translate back to the actual ram
            return self.write_8(addr - 0x2000, val);
        } else if addr < 0xFEA0 {
            // 160b Sprite Attribute Memory
            let offset = (addr - SPRITE_ATTRIBUTES_START) as usize;
            self.sprite_attributes[offset] = val;
        } else if addr < 0xFF00 {
            // FEA0-FEFF - Unmapped, do nothing.
            debug!("Write to unmapped addr {:#x}", addr);
        } else if addr < 0xFF80 {
            // FF00-FF7F - I/O Ports
            match addr {
                0xFF00 => {
                    // Select which inputs to read.
                    if (val & 0x20) == 0 {
                        self.input_read_mode = InputReadMode::Buttons;
                    } else if (val & 0x10) == 0 {
                        self.input_read_mode = InputReadMode::Direction;
                    }
                }
                0xFF04 => self.div = 0,
                0xFF05 => self.timer_tima = val,
                0xFF06 => self.timer_tma = val,
                0xFF07 => self.timer_tac = val,
                0xFF0F => self.interrupt_flags = val,
                0xFF40 => {
                    self.lcdc = val;
                    // If the LCD is off, LY should also be 0.
                    if val & 0x80 > 0 {
                        self.ly = 0;
                    }
                }
                0xFF41 => self.stat = val,
                0xFF42 => self.scy = val,
                0xFF43 => self.scx = val,
                0xFF44 => self.ly = val,
                0xFF46 => {
                    // DMA to OAM memory
                    for base in 0u16..=0x9Fu16 {
                        let src = ((val as u16) << 8) | base;
                        let dst = 0xFE00 | base;
                        self.write_8(dst, self.read_8(src));
                    }
                }
                0xFF47 => self.bg_palette = val,
                0xFF48 => self.sprite_palette_1 = val,
                0xFF49 => self.sprite_palette_2 = val,
                0xFF50 => self.use_boot = val == 0,
                _ => (),
            }
        } else if addr < 0xFFFF {
            // FF80-FFFE - High RAM (HRAM)
            let offset = (addr - 0xff80) as usize;
            self.ram1[offset] = val;
        } else {
            // 0xFFFF: Interrupt enabled register
            self.interrupt_enables = val;
        }
    }

    pub fn write_16(&mut self, addr: u16, val: u16) {
        self.write_8(addr, (val & 0xFF) as u8);
        self.write_8(addr + 1, ((val & 0xFF00) >> 8) as u8);
    }
}
