mod nrom;

use std::rc::Rc;
use std::cell::RefCell;

use super::rom;
use super::memory;

pub trait Mapper {
    fn prg(&self) -> Rc<RefCell<dyn memory::MemoryAccess>>;
    fn chr(&self) -> Rc<RefCell<dyn memory::MemoryAccess>>;
}

pub fn create_mapper(nes_file: rom::NesFile) -> Box<dyn Mapper> {
    match nes_file.mapper {
        0 => Box::new(nrom::Nrom::new(nes_file)),
        unknown => panic!("Unknown mapper type id={}", unknown)
    }
}
