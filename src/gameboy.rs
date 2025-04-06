use crate::cpu::CPU;
use crate::defs::{Button, Direction, Interrupt};
use crate::display::{Canvas, Display};
use crate::memory::Memory;

const SCANLINE_TICKS: u16 = 456;

pub struct Gameboy {
    cpu: CPU,
    memory: Memory,
    display: Display,

    ticks: u16,
    timer_ticks: u16,
    div_ticks: u16,
}

impl Default for Gameboy {
    fn default() -> Gameboy {
        Gameboy {
            cpu: Default::default(),
            display: Default::default(),
            memory: Default::default(),
            ticks: 0,
            timer_ticks: 0,
            div_ticks: 0,
        }
    }
}

impl Gameboy {
    pub fn step<T: Canvas>(&mut self, canvas: &mut T, tile_canvas: Option<&mut T>) {
        let cycles = self.cpu.exec(&mut self.memory) as u16;
        self.ticks += cycles;
        self.update_timer(cycles);

        if self.ticks >= SCANLINE_TICKS {
            self.display.render_line(&mut self.memory, canvas);
            self.ticks -= SCANLINE_TICKS;

            if tile_canvas.is_some() && self.memory.ly == 0 {
                self.display.dump_tiles(&self.memory, tile_canvas.unwrap());
            }
        }
    }

    pub fn button_pressed(&mut self, button: Button) {
        self.memory.set_button(button);
    }
    
    pub fn button_released(&mut self, button: Button) {
        self.memory.clear_button(button);
    }

    pub fn direction_pressed(&mut self, dir: Direction) {
        self.memory.set_direction(dir);
    }

    pub fn direction_released(&mut self, dir: Direction) {
        self.memory.clear_direction(dir);
    }

    fn update_timer(&mut self, cycles: u16) {
        self.timer_ticks += cycles;
        self.div_ticks += cycles;

        if self.div_ticks >= 256 {
            self.div_ticks -= 256;

            if self.memory.div == 0xFF {
                self.memory.div = 0;
            } else {
                self.memory.div += 1;
            }
        }

        if self.memory.timer_tac & 0x4 == 0 {
            // Timer disabled
            return;
        }

        let target_ticks: u16 = match self.memory.timer_tac & 0x2 {
            0b00 => 1024,
            0b01 => 16,
            0b10 => 64,
            0b11 => 256,
            _ => panic!(), // Impossible, but keeps the compiler happy
        };

        if self.timer_ticks >= target_ticks {
            self.timer_ticks -= target_ticks;
            if self.memory.timer_tima == 0xFF {
                self.memory.set_intterupt(Interrupt::Timer);
                self.memory.timer_tima = self.memory.timer_tma; 
            } else {
                self.memory.timer_tima += 1;
            }
        }
    }
}
