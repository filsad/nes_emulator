use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};

const RAM_SIZE: u16 = 0x0800;
const RAM_END: u16 = 0x1fff;
const PPU_ADDR: u16 = 0x2000;
const PPU_END: u16 = 0x3fff;
const PPU_SIZE: u16 = 8;
const APU_IO_ADDR: u16 = 0x4000;
const APU_IO_END: u16 = 0x4017;
const APU_IO_SIZE: u16 = 0x18;
const APU_TEST_ADDR: u16 = 0x4018;
const APU_TEST_END: u16 = 0x401f;
const CART_ADDR: u16 = 0x4020;
const MEM_END: u16 = 0xffff;

const VRAM_PALETTE_START: u16 = 0x3f00;
const VRAM_PALETTE_SIZE: u16 = 0x20;

pub trait MemoryAccess {
    fn read_b(&self, addr: u16) -> u8;
    fn write_b(&mut self, addr: u16, value: u8);
    fn read_w(&self, addr: u16) -> u16;
    fn write_w(&mut self, addr: u16, value: u16);
}

impl<T> MemoryAccess for T
where 
    T: Index<usize> + IndexMut<usize> + ?Sized,
    T::Output: Into<u8> + Into<u16> + From<u8> + Copy
{
    fn read_b(&self, addr: u16) -> u8 {
        self[addr as usize].into()
    }

    fn write_b(&mut self, addr: u16, value: u8) {
        self[addr as usize] = value.into();
    }

    fn read_w(&self, addr: u16) -> u16 {
        let lower: u16 = self[addr as usize].into();
        let upper: u16 = self[((addr + 1) & 0xffff) as usize].into();
        (upper << 8) | lower
    }

    fn write_w(&mut self, addr: u16, value: u16) {
        self[addr as usize] = ((value & 0xff) as u8).into();
        self[((addr + 1) & 0xffff) as usize] = ((value >> 8) as u8).into();
    }
}

pub struct MemoryRegion {
    mem: Vec<u8>,
    mem_start: usize,
}

impl MemoryRegion {
    pub fn new(size: u16, mem_start: u16, init_val: u8) -> MemoryRegion {
        MemoryRegion { mem: vec![init_val; size as usize], mem_start: mem_start as usize }
    }
    pub fn from_vec(source: Vec<u8>, mem_start: u16) -> MemoryRegion {
        MemoryRegion { mem: source, mem_start: mem_start as usize }
    }

    fn calculate_index(&self, addr: usize) -> usize {
        (addr - self.mem_start) % self.mem.len()
    }
}

impl Index<usize> for MemoryRegion {
    type Output = u8;
    fn index(&self, ind: usize) -> &Self::Output {
        let addr = self.calculate_index(ind);
        &self.mem[addr]
    }
}

impl IndexMut<usize> for MemoryRegion {
    fn index_mut(&mut self, ind: usize) -> &mut Self::Output {
        let addr = self.calculate_index(ind);
        &mut self.mem[addr]
    }
}

pub struct VideoMemoryRegion {
    mem_region: MemoryRegion,
    palette_region: MemoryRegion
}

impl VideoMemoryRegion {
    // fn new(size: u16, mem_start: u16, init_val: u8) -> VideoMemoryRegion {
    //     VideoMemoryRegion {
    //         mem_region: MemoryRegion::new(size, mem_start, init_val),
    //         palette_region: MemoryRegion::new(VRAM_PALETTE_SIZE, VRAM_PALETTE_START, 0)
    //     }
    // }
    pub fn from_vec(source: Vec<u8>, mem_start: u16) -> VideoMemoryRegion {
        let mem_region = MemoryRegion::from_vec(source, mem_start);
        let palette_region = MemoryRegion::new(VRAM_PALETTE_SIZE, VRAM_PALETTE_START, 0);
        VideoMemoryRegion { mem_region, palette_region }
    }
}

impl Index<usize> for VideoMemoryRegion {
    type Output = u8;
    fn index(&self, ind: usize) -> &Self::Output {
        if ind < VRAM_PALETTE_START as usize {
            &self.mem_region[ind]
        } else {
            let ind = if ind & 0x03 == 0 { 
                ind & 0xffef 
            } else { 
                ind
            };
            &self.palette_region[ind]
        }
    }
}

impl IndexMut<usize> for VideoMemoryRegion {
    fn index_mut(&mut self, ind: usize) -> &mut Self::Output {
        if ind < VRAM_PALETTE_START as usize {
            &mut self.mem_region[ind]
        } else {
            &mut self.palette_region[ind]
        }
    }
}

pub trait TrackableMemoryAccess : MemoryAccess {
    fn was_address_accessed(&mut self, addr: u16) -> bool;
    fn reset_access_state(&mut self);
}

pub struct TrackableMemoryRegion {
    mem_region: MemoryRegion,
    accessed: RefCell<Vec<bool>>,
}

impl TrackableMemoryRegion {
    pub fn new(size: u16, mem_start: u16, init_val: u8) -> TrackableMemoryRegion {
        let mem_region = MemoryRegion::new(size, mem_start, init_val);
        let accessed = RefCell::new(vec![false; size as usize]);
        TrackableMemoryRegion { mem_region, accessed }
    }
    // pub fn from_vec(source: Vec<u8>, mem_start: u16) -> TrackableMemoryRegion {
    //     let len = source.len();
    //     TrackableMemoryRegion { mem_region: MemoryRegion::from_vec(source, mem_start), accessed: RefCell::new(vec![false; len]) }
    // }
}

impl TrackableMemoryAccess for TrackableMemoryRegion {
    fn was_address_accessed(&mut self, addr: u16) -> bool {
        let index = self.mem_region.calculate_index(addr as usize);
        let accessed = self.accessed.borrow();
        accessed[index]
    }
    fn reset_access_state(&mut self) {
        self.accessed.borrow_mut().iter_mut().for_each(|x| *x = false);
    }
}

impl Index<usize> for TrackableMemoryRegion {
    type Output = <MemoryRegion as Index<usize>>::Output;
    fn index(&self, ind: usize) -> &Self::Output {
        let addr = self.mem_region.calculate_index(ind);
        self.accessed.borrow_mut()[addr] = true;
        &self.mem_region[ind]
    }
}

impl IndexMut<usize> for TrackableMemoryRegion {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let addr = self.mem_region.calculate_index(index);
        self.accessed.borrow_mut()[addr] = true;
        &mut self.mem_region[index]
    }
}

pub struct Memory {
    ram: Rc<RefCell<MemoryRegion>>, //[u8; RAM_SIZE],
    ppu: Rc<RefCell<TrackableMemoryRegion>>, //[u8; PPU_SIZE],
    apu_io: Rc<RefCell<TrackableMemoryRegion>>, //[u8; APU_IO_SIZE],
    mapper: Rc<RefCell<dyn MemoryAccess>>,
}

impl Memory {
    pub fn new(mapper: Rc<RefCell<dyn MemoryAccess>>) -> Box<Memory> {
        Box::new(Memory { 
            ram: Rc::new(RefCell::new(MemoryRegion::new(RAM_SIZE, 0, 0/*0xff*/))),
            ppu: Rc::new(RefCell::new(TrackableMemoryRegion::new(PPU_SIZE, PPU_ADDR, 0))),
            apu_io: Rc::new(RefCell::new(TrackableMemoryRegion::new(APU_IO_SIZE, APU_IO_ADDR, 0))),
            mapper
        })
    }

    pub fn get_region(&self, addr: u16) -> Ref<dyn MemoryAccess> {
        match addr {
            0..=RAM_END => self.ram.borrow(),
            PPU_ADDR..=PPU_END => self.ppu.borrow(),
            APU_IO_ADDR..=APU_IO_END => self.apu_io.borrow(),
            APU_TEST_ADDR..=APU_TEST_END => panic!("CPU Test Mode not supported"),
            CART_ADDR..=MEM_END => self.mapper.borrow()
        }
    }
    pub fn get_region_mut(&mut self, addr: u16) -> RefMut<dyn MemoryAccess> {
        match addr {
            0..=RAM_END => self.ram.borrow_mut(),
            PPU_ADDR..=PPU_END => self.ppu.borrow_mut(),
            APU_IO_ADDR..=APU_IO_END => self.apu_io.borrow_mut(),
            APU_TEST_ADDR..=APU_TEST_END => panic!("CPU Test Mode not supported"),
            CART_ADDR..=MEM_END => self.mapper.borrow_mut()
        }
    }

    pub fn get_ppu_registers(&self) -> Rc<RefCell<dyn TrackableMemoryAccess>> {
        self.ppu.clone()
    }
    pub fn get_apu_io_registers(&self) -> Rc<RefCell<dyn TrackableMemoryAccess>> {
        self.apu_io.clone()
    }
}

impl MemoryAccess for Memory {
    fn read_b(&self, addr: u16) -> u8 {
        let region = self.get_region(addr);
        region.read_b(addr)
    }

    fn write_b(&mut self, addr: u16, data: u8) {
        let mut region = self.get_region_mut(addr);
        region.write_b(addr, data);
    }

    fn read_w(&self, addr: u16) -> u16 {
        let lower = self.read_b(addr);
        let upper = self.read_b(addr + 1);
        (upper as u16) << 8 | lower as u16
    }

    fn write_w(&mut self, addr: u16, data: u16) {
        let upper = (data >> 8) as u8;
        let lower = (data & 0xff) as u8;
        self.write_b(addr, lower);
        self.write_b(addr + 1, upper);
    }
}
