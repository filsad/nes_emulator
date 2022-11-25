use std::fs;
use std::io::*;
use std::fmt;

fn make_error<T>(msg: &str) -> Result<T> {
    Err(Error::new(ErrorKind::InvalidInput, msg))
}

#[derive(Debug, Clone, Copy)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    FourScreen
}

pub struct NesFile {
    pub mapper: u8,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub nametable_mirroring: Mirroring,
    pub battery_mem: bool,
}

impl fmt::Debug for NesFile {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "<NesFile mapper={}, prg_rom={}k, chr_rom={}k, nametable_mirroring={:?}, battery_mem={}>",
            self.mapper,
            self.prg_rom.len() / 1024,
            self.chr_rom.len() / 1024,
            self.nametable_mirroring,
            self.battery_mem)?;
        Ok(())
    }
}

impl NesFile {
    pub fn load_from_file(path: &str) -> Result<NesFile> {
        static HEADER_SIZE: usize = 16;
        static PRG_ROM_SIZE_UNIT: usize = 16 * 1024;
        static CHR_ROM_SIZE_UNIT: usize = 8 * 1024;
        static TRAINER_SIZE: usize = 512;

        let mut file = fs::File::open(path)?;
        let mut buf: Vec<u8> = Vec::new();
        file.read_to_end(&mut buf)?;
        
        if buf.len() < HEADER_SIZE || buf[0..4] != [0x4e, 0x45, 0x53, 0x1a] {
            return make_error("Invalid NES file format.");
        }

        let prg_rom_size = PRG_ROM_SIZE_UNIT * buf[4] as usize;
        let chr_rom_size = CHR_ROM_SIZE_UNIT * buf[5] as usize;
        let mapper = buf[6] >> 4 | (buf[7] & 0xf0);
        let battery_mem = buf[6] & 0x02 != 0;
        let trainer_size = ((buf[6] & 0x04) >> 2) as usize * TRAINER_SIZE;
        let nametable_mirroring = match buf[6] {
            x if x & 0x08 != 0 => Mirroring::FourScreen,
            x if x & 0x01 != 0 => Mirroring::Vertical,
            x if x & 0x01 == 0 => Mirroring::Horizontal,
            _ => return make_error("Unsupported nametable mirroring mode")
        };
        
        let expected_buf_size = HEADER_SIZE + prg_rom_size + chr_rom_size + trainer_size;
        if buf.len() < expected_buf_size {
            return make_error("File too short");
        }
        let chr_rom = buf.split_off(HEADER_SIZE + trainer_size + prg_rom_size);
        let prg_rom = buf.split_off(HEADER_SIZE + trainer_size);

        Ok(NesFile { mapper, prg_rom, chr_rom, nametable_mirroring, battery_mem })
    }
}