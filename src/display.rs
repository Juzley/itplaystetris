use crate::defs::Interrupt;
use crate::memory::Memory;
use crate::utils::{from_twos_complement, unbounded_shr_u8};

use sdl3::pixels::Color;

const WINDOW_OFFSET: u8 = 7;

const VIEWPORT_HEIGHT: u8 = 144;
const VIEWPORT_WIDTH: u8 = 160;

const TILE_AREA_1_START: u16 = 0x8000;
const TILE_AREA_2_START: u16 = 0x8800;

const OAM_START: u16 = 0xFE00;
const SPRITE_LEN: u16 = 4; // 4 bytes per-sprite
const SPRITE_COUNT: u16 = 40; // 40 total bytes in the OAM area.
const MAX_SPRITES_PER_LINE: usize = 10;
const OAM_END: u16 = OAM_START + SPRITE_LEN * SPRITE_COUNT;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SpritePriority {
    High,
    Low,
}

#[derive(Debug)]
enum SpritePalette {
    Palette1,
    Palette2,
}

#[derive(Debug)]
struct Sprite {
    x: u8,
    y: u8,
    tile_index: u8,
    priority: SpritePriority,
    _flip_x: bool,
    _flip_y: bool,
    palette: SpritePalette,
}

impl Sprite {
    fn from_mem(mem: &Memory, addr: u16) -> Self {
        let flags = mem.read_8(addr + 3);

        Sprite {
            x: mem.read_8(addr + 1),
            y: mem.read_8(addr),
            tile_index: mem.read_8(addr + 2),
            priority: if (flags & 0x80) == 0 { SpritePriority::High } else { SpritePriority::Low },
            _flip_x: (flags & 0x20) > 0,
            _flip_y: (flags & 0x40) > 0,
            palette: if (flags & 0x10) > 0 { SpritePalette::Palette2 } else {SpritePalette::Palette1 },
        }
    }
}

pub enum TileArea {
    Area1,
    Area2,
}

impl TileArea {
    fn to_tile_offset(&self, tile_index: u8) -> u16 {
        let raw_offset = match self {
            TileArea::Area1 => TILE_AREA_1_START + (tile_index as u16) * 16,
            TileArea::Area2 => {
                TILE_AREA_2_START + (from_twos_complement(tile_index) as i32 * 16) as u16
            }
        };

        return raw_offset;
    }
}

pub enum TileView {
    View1,
    View2,
}

pub enum SpriteSize {
    Small,
    Large,
}

impl TileView {
    fn to_offset(&self) -> u16 {
        match self {
            TileView::View1 => 0x9800,
            TileView::View2 => 0x9C00,
        }
    }
}

pub trait Canvas {
    fn draw_pixel(&mut self, _x: u16, _y: u16, _colour: Color) {}
    fn present(&mut self) {}
    fn clear(&mut self) {}
}

pub struct Display {
    wx: u8,
    wy: u8,
}

impl Default for Display {
    fn default() -> Display {
        Display {
            wx: 0,
            wy: 0,
        }
    }
}

impl Display {
    fn apply_palette(colour: u8, palette: u8) -> u8 {
        return (palette >> (colour * 2)) & 0x2;
    }

    fn convert_colour(colour: u8) -> Color {
        match colour {
            1 => Color::RGB(139, 172, 15),
            2 => Color::RGB(48, 98, 48),
            3 => Color::RGB(15, 56, 15),
            _ => Color::RGB(155, 188, 15),
        }
    }

    fn fetch_pixel_val(mem: &Memory, addr: u16, pixel: u8, palette: u8) -> u8 {
        let low_byte = mem.read_8(addr);
        let high_byte = mem.read_8(addr + 1);

        let low_bit = unbounded_shr_u8(low_byte, 7 - pixel) & 0x1;
        let high_bit = unbounded_shr_u8(high_byte, 7 - pixel) & 0x1;

        return Self::apply_palette((high_bit << 1) | low_bit, palette);
    }

    fn fetch_bg_colour(&self, mem: &Memory, lx: u8) -> u8 {
        let lx = lx as u16;
        let ly = mem.ly as u16;
        let scx = mem.scx as u16;
        let scy = mem.scy as u16;

        let bg_tile_x = ((lx + scx) / 8) & 0x1f;
        let bg_tile_y = 32 * (((ly + scy) / 8) & 0xFF);
        let bg_tile_offset = (bg_tile_x + bg_tile_y) & 0x3FF;
        let bg_tile_index = mem.read_8(mem.lcdc_bg_tile_view().to_offset() + bg_tile_offset);
        let bg_tile_base = mem.lcdc_bg_tile_area().to_tile_offset(bg_tile_index);
        let bg_tile_addr = bg_tile_base + 2 * ((ly + scy) % 8);

        return Self::fetch_pixel_val(mem, bg_tile_addr, ((lx + scx) % 8) as u8, mem.bg_palette);
    }

    fn fetch_window_colour(&self, mem: &Memory, lx: u8) -> u8 {
        let lx = lx as u16;
        let wx = (self.wx - WINDOW_OFFSET) as u16;
        let line_count = (mem.ly - self.wy) as u16;

        let wnd_tile_x = (lx - wx) / 8;
        let wnd_tile_y = 32 * line_count / 8;
        let wnd_tile_offset = wnd_tile_x + wnd_tile_y & 0x3FF;
        let wnd_tile_index = mem.read_8(mem.lcdc_window_tile_view().to_offset() + wnd_tile_offset);

        let wnd_tile_base = mem.lcdc_bg_tile_area().to_tile_offset(wnd_tile_index);
        let wnd_tile_addr = wnd_tile_base + 2 * (line_count % 8);

        return Self::fetch_pixel_val(mem, wnd_tile_addr, ((lx - wx) % 8) as u8, mem.bg_palette);
    }

    pub fn dump_tiles<T: Canvas>(&self, mem: &Memory, canvas: &mut T) {
        canvas.clear();
        for tile_x in 0..16 {
            for tile_y in 0..24 {
                // 16 bytes per tile
                let tile_offset = (16 * (tile_y * 16 + tile_x)) as u16;
                let tile_start = TILE_AREA_1_START + tile_offset;
                
                for pixel_y in 0..8 {
                    let addr = tile_start + pixel_y * 2;
                    let low_byte = mem.read_8(addr);
                    let high_byte = mem.read_8(addr + 1);

                    for pixel_x in 0..8 {
                        // 2 bytes per line
                        let low = unbounded_shr_u8(low_byte, 7 - pixel_x) & 0x1;
                        let high = unbounded_shr_u8(high_byte, 7 - pixel_x) & 0x1;
                        let val = Self::apply_palette((high << 1) | low, mem.bg_palette);
                        canvas.draw_pixel(
                            tile_x * 8 + pixel_x,
                            tile_y * 8 + pixel_y,
                            Self::convert_colour(val),
                        );
                    }
                }
            }
        }
        canvas.present();
    }
    
    fn fetch_sprites(&self, mem: &Memory) -> Vec<Sprite> {
        let mut sprites: Vec<Sprite> = Vec::new();

        if !mem.lcdc_sprite_enable() {
            return sprites;
        }

        let sprite_size: u8 = match mem.lcdc_sprite_size() {
            SpriteSize::Large => 16,
            SpriteSize::Small => 8,
        };

        for addr in (OAM_START..OAM_END).step_by(SPRITE_LEN.into()) {
            let cur_sprite = Sprite::from_mem(mem, addr);

            if cur_sprite.x == 0 {
                continue;
            }
            
            if mem.ly + 16 <= cur_sprite.y {
                continue;
            }

            if mem.ly + 16 >= cur_sprite.y + sprite_size {
                continue;
            }

            sprites.push(cur_sprite);
            
            if sprites.len() == MAX_SPRITES_PER_LINE {
                break;
            }
        }

        return sprites;
    }

    fn get_sprite_colour(&self, mem: &Memory, sprites: &Vec<Sprite>, lx: u8) -> Option<(u8, SpritePriority)> {
        for sprite in sprites {
            if sprite.x < lx + 8 && lx <= sprite.x {
                let tile_base = TileArea::Area1.to_tile_offset(sprite.tile_index);
                let tile_addr = tile_base + 2u16 * ((mem.ly - sprite.y + 16) as u16);
                let palette = match sprite.palette {
                    SpritePalette::Palette1 => mem.sprite_palette_1,
                    SpritePalette::Palette2 => mem.sprite_palette_2,
                };

                let colour = Self::fetch_pixel_val(mem, tile_addr, lx - sprite.x + 8, palette);
                return Some((colour, sprite.priority));
            }
        }

        None
    }

    fn mix_colours(bg_colour: u8, sprite_data: Option<(u8, SpritePriority)>) -> u8 {
        match sprite_data {
            Some((sprite_colour, sprite_pri)) => {
                if sprite_colour == 0 {
                    return bg_colour;
                }
                if sprite_pri == SpritePriority::Low && bg_colour > 0 {
                    return bg_colour;
                }
                return sprite_colour;
            }
            None => bg_colour,
        }
    }


    pub fn render_line<T: Canvas>(&mut self, mem: &mut Memory, canvas: &mut T) {
        // Refs: https://hacktix.github.io/GBEDG/ppu
        //       https://www.youtube.com/watch?v=8TVgN16DrEU

        if !mem.lcdc_enable() {
            return;
        }

        if mem.ly < VIEWPORT_HEIGHT {
            let sprites = if mem.lcdc_sprite_enable() {
                self.fetch_sprites(mem)
            } else {
                vec![]
            };

            for lx in 0..VIEWPORT_WIDTH {
                let mut bg_colour = 0;
                if mem.lcdc_window_enable() && self.wy <= mem.ly && self.wx <= lx + WINDOW_OFFSET {
                    // Draw the window
                    bg_colour = self.fetch_window_colour(mem, lx);
                } else if mem.lcdc_bg_enable() {
                    // Draw the background.
                    bg_colour = self.fetch_bg_colour(mem, lx);
                }

                let sprite_data = self.get_sprite_colour(mem, &sprites, lx);
                let colour = Self::mix_colours(bg_colour, sprite_data);
                
                canvas.draw_pixel(lx.into(), mem.ly.into(), Self::convert_colour(colour));
            }
        }

        mem.ly += 1;
        if mem.ly == 144 {
            mem.set_intterupt(Interrupt::VBlank);
            canvas.present();
        } else if mem.ly == 153 {
            mem.ly = 0;
        }
    }
}
