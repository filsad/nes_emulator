use std::rc::Rc;
use std::cell::RefCell;

use super::rom;
use super::memory;

pub struct Nrom {
    prg: Rc<RefCell<memory::MemoryRegion>>,
    chr: Rc<RefCell<memory::VideoMemoryRegion>>
}

impl Nrom {
    pub fn new(nes_file: rom::NesFile) -> Nrom {
        Nrom { prg: Rc::new(RefCell::new(memory::MemoryRegion::from_vec(nes_file.prg_rom, 0x8000))), 
               chr: Rc::new(RefCell::new(memory::VideoMemoryRegion::from_vec(nes_file.chr_rom, 0))) }
    }
}

impl super::Mapper for Nrom {
    fn prg(&self) -> Rc<RefCell<dyn memory::MemoryAccess>> {
        self.prg.clone()
    }
    fn chr(&self) -> Rc<RefCell<dyn memory::MemoryAccess>> {
        self.chr.clone()
    }
}