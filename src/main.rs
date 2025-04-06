mod boot;
mod defs;
mod cartridge;
mod cpu;
mod display;
mod gameboy;
mod memory;
mod utils;

use sdl3;
use sdl3::event::Event;
use sdl3::keyboard::Keycode;
use sdl3::pixels::Color;
use sdl3::rect::Point;
use sdl3::render::WindowCanvas;

use display::Canvas;
use gameboy::Gameboy;

impl Canvas for WindowCanvas {
    fn draw_pixel(&mut self, x: u16, y: u16, colour: Color) {
        self.set_draw_color(colour);
        let _ = self.draw_point(Point::new(x.into(), y.into()));
    }

    fn present(&mut self) {
        self.present();
    }

    fn clear(&mut self) {
        self.clear();
    }
}

fn main() {
    env_logger::init();

    let sdl_context = sdl3::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("Gameboy", 160, 144)
        .position_centered()
        .build()
        .unwrap();

    let display = window.get_display().unwrap();
    let display_mode = display.get_mode().unwrap();

    let mut canvas = window.into_canvas();
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let tiles_window = video_subsystem
        .window("Tiles", 128, 192)
        .position(0, 0)
        .build()
        .unwrap();

    let mut tiles_canvas = tiles_window.into_canvas();
    tiles_canvas.set_draw_color(Color::RGB(0, 0, 0));
    tiles_canvas.clear();
    tiles_canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut gb: Gameboy = Default::default();

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                Event:: KeyDown { keycode, ..} =>
                    match keycode {
                        Some(Keycode::Escape) => break 'running,
                        Some(Keycode::Up) => gb.direction_pressed(defs::Direction::Up),
                        Some(Keycode::Down) => gb.direction_pressed(defs::Direction::Down),
                        Some(Keycode::Right) => gb.direction_pressed(defs::Direction::Right),
                        Some(Keycode::Left) => gb.direction_pressed(defs::Direction::Left),
                        Some(Keycode::Return) => gb.button_pressed(defs::Button::Start),
                        Some(Keycode::Space) => gb.button_pressed(defs::Button::Select),
                        Some(Keycode::Z) => gb.button_pressed(defs::Button::B),
                        Some(Keycode::X) => gb.button_pressed(defs::Button::A),
                        _ => (),
                    }
                Event:: KeyUp { keycode, ..} =>
                    match keycode {
                        Some(Keycode::Up) => gb.direction_released(defs::Direction::Up),
                        Some(Keycode::Down) => gb.direction_released(defs::Direction::Down),
                        Some(Keycode::Right) => gb.direction_released(defs::Direction::Right),
                        Some(Keycode::Left) => gb.direction_released(defs::Direction::Left),
                        Some(Keycode::Return) => gb.button_released(defs::Button::Start),
                        Some(Keycode::Space) => gb.button_released(defs::Button::Select),
                        Some(Keycode::Z) => gb.button_released(defs::Button::B),
                        Some(Keycode::X) => gb.button_released(defs::Button::A),
                        _ => (),
                    }
                _ => {}
            }
        }

        gb.step(&mut canvas, Some(&mut tiles_canvas));
    }
}
