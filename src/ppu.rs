use std::rc::Rc;
use std::cell::RefCell;

use bitvec::prelude::*;

use super::memory;
use super::bus;


const PPUCTRL: u16 = 0x2000;
const _PPUMASK: u16 = 0x2001;
const PPUSTATUS: u16 = 0x2002;
const OAMADDR: u16 = 0x2003;
const OAMDATA: u16 = 0x2004;
const PPUSCROLL: u16 = 0x2005;
const PPUADDR: u16 = 0x2006;
const PPUDATA: u16 = 0x2007;

const GENERATE_NMI: u8 = 0x80;
const SPRITE_SIZE: u8 = 0x20;
const BKG_PATTERN_ADDR: u8 = 0x10;
const SPRITE_PATTERN_ADDR: u8 = 0x08;
const VRAM_ADDR_INCREMENT: u8 = 0x04;
const BASE_NT_ADDR: u8 = 0x03;

const V_BLANK_STARTED: u8 = 0x80;

pub struct Ppu {
    vram: Rc<RefCell<dyn memory::MemoryAccess>>,
    regs: Rc<RefCell<dyn memory::TrackableMemoryAccess>>,
    bus: Rc<RefCell<bus::Bus>>,

    _oam: [u8; 256],
    oam_addr: u8,
    oam_data: u8,

    v: BitArr!(for 15, in u16, Lsb0), // v: [bool; 15],
    t: BitArr!(for 15, in u16, Lsb0), // t: [bool; 15],
    x: BitArr!(for 3, in u8, Lsb0), // x: [bool; 3],
    w: bool,

    _render_sprite: bool,
    _redner_bkg: bool,
    generate_nmi: bool,
    sprite_8x16: bool,
    bkg_pattern_addr: bool,
    sprite_pattern_addr: bool,
    vram_addr_increment_32: bool,

    _sprite_overflow: bool,
    _sprite_0_hit: bool,
    vertical_blank: bool,

    nt_byte: [u8; 2],
    attribute_byte: [u8; 2],
    pattern_low: [u8; 2],
    pattern_high: [u8; 2],
    attribute_low: u8,
    attribute_high: u8,

    image: [[u8; 256]; 240],

    frame: u64,
    scanline: u16,
    cycle: u16,
}

impl Ppu {
    pub fn new(vram: Rc<RefCell<dyn memory::MemoryAccess>>, regs: Rc<RefCell<dyn memory::TrackableMemoryAccess>>, bus: Rc<RefCell<bus::Bus>>) -> Ppu {
        Ppu {
            vram, regs, bus,
            _oam: [0; 256], oam_addr: 0, oam_data: 0,
            v: bitarr![u16, Lsb0; 0; 15],
            t: bitarr![u16, Lsb0; 0; 15],
            x: bitarr![u8, Lsb0; 0; 3],
            w: false, _render_sprite: false, _redner_bkg: false, generate_nmi: false, sprite_8x16: false, bkg_pattern_addr: false, sprite_pattern_addr: false, vram_addr_increment_32: false,
            _sprite_overflow: false, _sprite_0_hit: false, vertical_blank: false,
            nt_byte: [0; 2], attribute_byte: [0; 2], pattern_low: [0; 2], pattern_high: [0; 2],
            attribute_low: 0, attribute_high: 0,
            image: [[0; 256]; 240],
            frame: 0,
            scanline: 261,
            cycle: 0,
        }
    }
    pub fn tick(&mut self) {
        self.read_registers();

        match self.scanline {
            0..=239 => self.regular_scanline(),
            240 => (),
            241 => if self.cycle == 1 {
                let mut bus = self.bus.borrow_mut();
                bus.nmi_asserted = true;
                bus.frame_finished = true;
                self.vertical_blank = true;
            },
            242..=260 => (),
            261 => self.prerender_scanline(),
            _ => ()
        };
        
        self.increment_counters();
        self.write_registers();
    }
    pub fn get_image(&self) -> &[[u8; 256]] {
        &self.image[..]
    }

    /* Private methods */

    fn regular_scanline(&mut self) {
        match self.cycle {
            0 => (),
            1..=256 => {
                self.put_pixel();
                self.cycles_fetch();
            },
            257 => {
                self.v[0..=4].clone_from_bitslice(&self.t[0..=4]);
                self.v[10..=10].clone_from_bitslice(&self.t[10..=10]);
            },
            258..=320 => (), // Sprites' fetches
            321..=336 => self.cycles_fetch(),
            337..=340 => (), // Dummy nametable fetches
            _ => panic!("Cycle out of bounds")
        }
    }
    fn prerender_scanline(&mut self) {
        match self.cycle {
            0 => (),
            1 => { 
                self.bus.borrow_mut().nmi_asserted = false;
                self.vertical_blank = false;
                self.cycles_fetch()
            },
            2..=256 => self.cycles_fetch(),
            257 => {
                self.v[0..=4].clone_from_bitslice(&self.t[0..=4]);
                self.v[10..=10].clone_from_bitslice(&self.t[10..=10]);
            },
            258..=279 => (),
            280..=304 => {
                self.v[5..=9].clone_from_bitslice(&self.t[5..=9]);
                self.v[11..=14].clone_from_bitslice(&self.t[11..=14]);
            },
            305..=320 => (),
            321..=336 => self.cycles_fetch(),
            337..=340 => (), // Dummy nametable fetches
            _ => panic!("Cycle out of bounds")
        }
    }

    fn cycles_fetch(&mut self) {
        match self.cycle % 8 {
            1 | 3 | 5 | 7 => (),
            2 => self.fetch_nt_byte(),
            4 => self.fetch_attr_byte(),
            6 => self.fetch_pattern_low(),
            0 => {
                self.fetch_pattern_high();
                if self.cycle != 256 {
                    self.increment_x();
                } else {
                    self.increment_y();
                }
            },
            _ => panic!("Wrong fetch operation")
        }
        self.shift_regs();
    }

    fn read_registers(&mut self) {
        let mut registers = self.regs.borrow_mut();
        if registers.was_address_accessed(PPUSTATUS) {
            let mut ppustatus_reg = registers.read_b(PPUSTATUS);
            ppustatus_reg &= !V_BLANK_STARTED;
            registers.write_b(PPUSTATUS, ppustatus_reg);

            self.vertical_blank = false;
            self.w = false; 
        }

        if registers.was_address_accessed(PPUCTRL) {
            let ctrl_reg = registers.read_b(PPUCTRL);
            self.generate_nmi = ctrl_reg & GENERATE_NMI != 0;
            self.sprite_8x16 = ctrl_reg & SPRITE_SIZE != 0;
            self.bkg_pattern_addr = ctrl_reg & BKG_PATTERN_ADDR != 0;
            self.sprite_pattern_addr = ctrl_reg & SPRITE_PATTERN_ADDR != 0;
            self.vram_addr_increment_32 = ctrl_reg * VRAM_ADDR_INCREMENT != 0;
            
            let nt_addr = ctrl_reg & BASE_NT_ADDR;
            self.t[10..=11].store(nt_addr);
        }
        if registers.was_address_accessed(OAMADDR) {
            self.oam_addr = registers.read_b(OAMADDR);
        }
        if registers.was_address_accessed(OAMDATA) {
            self.oam_data = registers.read_b(OAMDATA);
            self.oam_addr += 1;
        }
        if registers.was_address_accessed(PPUSCROLL) {
            let ppuscroll_reg = registers.read_b(PPUSCROLL);
            if self.w {
                self.t[5..=9].store(ppuscroll_reg >> 3);
                self.t[12..=14].store(ppuscroll_reg >> 0);
            } else {
                self.x[..].store(ppuscroll_reg);
                self.t[0..=4].store(ppuscroll_reg >> 3);
            }
            self.w = !self.w;
        }
        if registers.was_address_accessed(PPUADDR) {
            let ppuaddr_reg = registers.read_b(PPUADDR);
            if self.w {
                self.t[0..=7].store(ppuaddr_reg);
                self.v = self.t;
            } else {
                self.t[8..=13].store(ppuaddr_reg);
                self.t.set(14, false);
            }
            self.w = !self.w;
        }
        if registers.was_address_accessed(PPUDATA) {
            let ppudata_reg = registers.read_b(PPUDATA);
            let increment: u16 = if self.vram_addr_increment_32 { 32 } else { 1 };
            let v: u16 = self.v[..].load();
            self.vram.borrow_mut().write_b(v, ppudata_reg);
            self.v[..].store(v + increment);
        }
        // if registers.was_address_accessed(PPUMASK) {
        //     let ppumask_reg = registers.read_b(PPUMASK);
            
        // }
    }

    fn write_registers(&mut self) {
        let mut registers = self.regs.borrow_mut();

        if self.vertical_blank {
            let mut ppustatus_reg = registers.read_b(PPUSTATUS);
            ppustatus_reg |= V_BLANK_STARTED;
            registers.write_b(PPUSTATUS, ppustatus_reg);
        }

        registers.write_b(PPUDATA, self.vram.borrow().read_b(self.v[..].load()));
        registers.reset_access_state();
    }

    fn fetch_nt_byte(&mut self) {
        let v: u16 = self.v[..].load();
        self.nt_byte[0] = self.nt_byte[1];
        self.nt_byte[1] = self.vram.borrow().read_b(0x2000 | (v & 0x2fff));
    }
    fn fetch_attr_byte(&mut self) {
        let v: u16 = self.v[..].load();
        let addr = 0x23c0 | (v & 0x0c00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
        self.attribute_byte[0] = self.attribute_byte[1];
        self.attribute_byte[1] = self.vram.borrow().read_b(addr);
    }

    fn calculate_pattern_addr(&self) -> u16 {
        let fine_y = (self.v[..].load::<u16>() & 0x7000) >> 12;
        let pattern_table_select = (self.sprite_pattern_addr as u16) << 12;
        pattern_table_select | (self.nt_byte[0] as u16) << 4 | fine_y
    }
    fn fetch_pattern_low(&mut self) {
        // self.pattern_low[0] = self.pattern_low[1];
        self.pattern_low[1] = self.vram.borrow().read_b(self.calculate_pattern_addr());
    }
    fn fetch_pattern_high(&mut self) {
        let high_addr = self.calculate_pattern_addr() | 0x08;
        // self.pattern_high[0] = self.pattern_high[1];
        self.pattern_high[1] = self.vram.borrow().read_b(high_addr);
    }

    fn select_attribute(&self) -> u8 {
        let vx = self.v[0..5].load::<u8>() % 4;
        let vy = self.v[5..10].load::<u8>() % 4;
        match (vx, vy) {
            (0..=1, 0..=1) => self.attribute_byte[0] & 0x03,
            (2..=3, 0..=1) => (self.attribute_byte[0] >> 2) & 0x03,
            (0..=1, 2..=3) => (self.attribute_byte[0] >> 4) & 0x03,
            (2..=3, 2..=3) => (self.attribute_byte[0] >> 6) & 0x03,
            _ => panic!("Wrong attribute index")
        }
    }
    fn shift_regs(&mut self) {
        // self.mux <<= 1;
        // self.mux |= shl(&mut self.nt_byte[0]);

        self.pattern_low[0] <<= 1;
        self.pattern_low[0] |= shl(&mut self.pattern_low[1]);
        self.pattern_high[0] <<= 1;
        self.pattern_high[0] |= shl(&mut self.pattern_high[1]);

        let selected_attribute = self.select_attribute();
        self.attribute_low <<= 1;
        self.attribute_low |= selected_attribute & 0x01;
        self.attribute_high <<= 1;
        self.attribute_high |= selected_attribute >> 1;
    }

    fn put_pixel(&mut self) {
        let x: u8 = self.x[..].load();
        let pattern = ((self.pattern_high[0] >> x) << 1) & 0x02 | ((self.pattern_low[0] >> x) & 0x01);
        let attribute = ((self.attribute_high >> x) << 1) & 0x02 | ((self.attribute_low >> x) & 0x01);
        let palette_addr = 0x3f00 | (if pattern == 0 { 0 } else { attribute * 4 + pattern } as u16);
        self.image[self.scanline as usize][self.cycle as usize - 1] = self.vram.borrow().read_b(palette_addr);
    }

    fn increment_x(&mut self) {
        let mut v: u16 = self.v[..].load();
        if (v & 0x001f) == 31 {
            v &= !0x001f;
            v ^= 0x0400;
        } else {
            v += 1;
        }
        self.v[..].store(v);
    }
    fn increment_y(&mut self) {
        let mut v: u16 = self.v[..].load();
        if v & 0x7000 != 0x7000 {
            v += 0x1000;
        } else {
            v &= !0x7000;
            let mut y = (v & 0x03e0) >> 5;
            if y == 29 {
                y = 0;
                v ^= 0x0800;
            } else if y == 31 {
                y = 0;
            } else {
                y += 1;
            }
            v = (v & !0x03e0) | (y << 5);
        }
        self.v[..].store(v);
    }
    
    fn increment_counters(&mut self) {
        self.cycle += 1;
        if self.cycle > 340 {
            self.cycle = 0;
            self.scanline += 1;
        }
        if self.scanline == 261 && (self.cycle == 340 ||
           (self.cycle == 339 && self.frame % 2 == 1)) {
            self.cycle = 0;
            self.scanline = 0;
            self.frame += 1;
        }
    }
}

fn shl(reg: &mut u8) -> u8 {
    let bit_val = *reg >> 7;
    *reg <<= 1;
    bit_val
}
