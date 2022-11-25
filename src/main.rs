use std::{result::Result, error, env, io, fs};
use std::rc::Rc;
use std::cell::RefCell;

#[macro_use] extern crate log;
use sdl2::video::Window;
use sdl2::render::Canvas;

mod rom;
mod mapper;
mod cpu;
mod memory;
mod opcodes;
mod ppu;
mod bus;

fn main() -> Result<(), Box<dyn error::Error>> {
    setup_logger()?;
    let arg = env::args().nth(1);
    let file_name = match arg {
        Some(rom) => rom,
        None => panic!("Specify NES rom file as an argument")
    };

    let palette = &fs::read("bin/nespalette.pal")?[..];
    let mut canvas = init_canvas()?;
    
    let rom = rom::NesFile::load_from_file(&file_name)?;
    info!("ROM file loaded {:?}", rom);
    let mapper = mapper::create_mapper(rom);
    let memory = memory::Memory::new(mapper.prg());
    let bus = Rc::new(RefCell::new(bus::Bus::new()));
    let mut ppu = ppu::Ppu::new(mapper.chr(), memory.get_ppu_registers(), bus.clone());
    let mut cpu = cpu::Cpu::new(memory, bus.clone());
    // cpu.pc = 0xc000;
    loop {
        cpu.tick();
        for _ in 0..3 {
            ppu.tick();
        }
        {
            let frame_finished_ref = &mut bus.borrow_mut().frame_finished;
            if *frame_finished_ref {
                draw_frame(&mut canvas, ppu.get_image(), palette);
                *frame_finished_ref = false;
            }
        }
        if cpu.cycle % 1_000_000 /*9484*/ == 0 {
            println!("Klik!");
        }
    }
    // Ok(())
}

fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new().format(|out, msg, _record| 
            out.finish(format_args!("{}", msg))
        )
        .chain(fern::Dispatch::new()
            .level(log::LevelFilter::Trace)
            .chain(
                fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open("rustnes.log")?)
        )
        .chain(fern::Dispatch::new()
            .level(log::LevelFilter::Warn)
            // .level(log::LevelFilter::Trace)
            .chain(io::stderr())
        )
        .apply()?;
    Ok(())
}

fn init_canvas() -> Result<Canvas<Window>, Box<dyn error::Error>> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let window = video_subsystem.window("RustNES", 640, 480)
        .position_centered()
        .build()?;
    let mut canvas = window.into_canvas().build()?;
    canvas.set_draw_color(sdl2::pixels::Color::BLACK);
    canvas.clear();
    canvas.present();
    Ok(canvas)
}

fn draw_frame(canvas: &mut Canvas<Window>, data: &[[u8; 256]], palette: &[u8]) {
    for (line_num, &line) in data.iter().enumerate() {
        for (column_num, &color) in line.iter().enumerate() {
            let color = color as usize;
            canvas.set_draw_color(sdl2::pixels::Color::RGB(palette[color*3], palette[color*3 + 1], palette[color*3 + 2]));
            canvas.draw_point((column_num as i32, line_num as i32)).unwrap();
        }
    }
    canvas.present();
}
