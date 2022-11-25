extern crate wrapping_arithmetic;
use wrapping_arithmetic::wrappit;

use std::rc::Rc;
use std::cell::RefCell;

use super::memory::*;
use super::opcodes;
use super::bus;

const CARRY: usize = 0;
const ZERO: usize = 1;
const INT_DISABLE: usize = 2;
const DECIMAL: usize = 3;
const B_FLAG_0: usize = 4;
const B_FLAG_1: usize = 5;
const OVERFLOW: usize = 6;
const NEGATIVE: usize = 7;

const STACK_PAGE: u16 = 0x0100;
const NMI_VECTOR: u16 = 0xfffa;
const RESET_VECTOR: u16 = 0xfffc;
const IRQ_VECTOR: u16 = 0xfffe;

pub struct Cpu {
    mem: Box<dyn MemoryAccess>,
    bus: Rc<RefCell<bus::Bus>>,
    pub cycle: u64,
    a: u8,
    x: u8,
    y: u8,
    pub pc: u16,
    s: u8,
    flags: u8,

    cycles_to_go: u8,
}

impl Cpu {
    pub fn new(mem: Box<dyn MemoryAccess>, bus: Rc<RefCell<bus::Bus>>) -> Self {
        let mut cpu = Cpu { mem, bus, cycle: 0, a: 0, x: 0, y: 0, pc: 0, s: 0xfd, flags: 0x24, cycles_to_go: 6 };
        let start_addr = cpu.mem.read_w(RESET_VECTOR);
        cpu.pc = start_addr;
        cpu
    }

    pub fn tick(&mut self) {
        self.cycle += 1;
        if self.cycles_to_go > 0 {
            self.cycles_to_go -= 1;
            return;
        }

        self.nmi();
        let opcode = self.fetch_opcode();
        self.cycles_to_go = opcode.cycles - 1;
        self.debug_format(opcode);
        self.decode(opcode);
    }

    // Private functions

    fn nmi(&mut self) {
        let vblank = self.bus.borrow().nmi_asserted;
        if vblank {
            self.push_w(self.pc);
            self.push_b(self.flags);
            self.pc = self.mem.read_w(NMI_VECTOR);
            self.bus.borrow_mut().nmi_asserted = false;
        }
    }

    #[wrappit]
    fn debug_format(&self, opcode: &opcodes::Opcode)
    {
        use opcodes::AddressingMode::*;

        // let registers = format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{zero:>3},{zero:>3} CYC:{}", self.a, self.x, self.y, self.flags, self.s, self.cycle, zero = 0);
        let registers = format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}  CYC:{}", self.a, self.x, self.y, self.flags, self.s, self.cycle);
        let old_pc = self.pc - 1;
        let opc = self.mem.read_b(old_pc);
        let mut operand = "".into();
        let mut instr_operand = "".into();

        match opcode.mode {
            Acc => {
                instr_operand = "A".into();
            },
            Immediate => {
                let addr = self.mem.read_b(self.pc);
                operand = format!("{:02X}", addr);
                instr_operand = format!("#${}", operand);
            },
            ZeroPage => {
                let addr = self.mem.read_b(self.pc);
                operand = format!("{:02X}", addr);
                instr_operand = format!("${} = {:02X}", operand, self.mem.read_b(addr as u16));
            },
            ZeroPageX => {
                let addr1 = self.mem.read_b(self.pc);
                let addr = (addr1 + self.x) as u16;
                operand = format!("{:02X}", addr1);
                instr_operand = format!("${},X @ {:02X} = {:02X}", operand, addr, self.mem.read_b(addr));
            },
            ZeroPageY => {
                let addr1 = self.mem.read_b(self.pc);
                let addr = (addr1 + self.y) as u16;
                operand = format!("{:02X}", addr1);
                instr_operand = format!("${},Y @ {:02X} = {:02X}", operand, addr, self.mem.read_b(addr));
            },
            Relative => {
                let displacement = self.mem.read_b(self.pc) as i8;
                operand = format!("{:02X}", displacement as u8);
                instr_operand = format!("${:04X}", self.pc as i32 + displacement as i32 + 1);
            },
            Absolute => {
                let addr_lower = self.mem.read_b(self.pc);
                let addr_upper = self.mem.read_b(self.pc + 1);
                let addr = (addr_upper as u16) << 8 | addr_lower as u16;
                operand = format!("{:02X} {:02X}", addr_lower, addr_upper);
                if opc == 0x4c || opc == 0x20 {
                    instr_operand = format!("${:04X}", addr);
                } else {
                    instr_operand = format!("${:04X} = {:02X}", addr, self.mem.read_b(addr));
                }
            },
            AbsoluteX => {
                let addr_lower = self.mem.read_b(self.pc);
                let addr_upper = self.mem.read_b(self.pc + 1);
                let addr = (addr_upper as u16) << 8 | addr_lower as u16;
                let addr2 = addr + self.x as u16;
                operand = format!("{:02X} {:02X}", addr_lower, addr_upper);
                instr_operand = format!("${:04X},X @ {:04X} = {:02X}", addr, addr2, self.mem.read_b(addr2));
            },
            AbsoluteY => {
                let addr_lower = self.mem.read_b(self.pc);
                let addr_upper = self.mem.read_b(self.pc + 1);
                let addr = (addr_upper as u16) << 8 | addr_lower as u16;
                let addr2 = addr + self.y as u16;
                operand = format!("{:02X} {:02X}", addr_lower, addr_upper);
                instr_operand = format!("${:04X},Y @ {:04X} = {:02X}", addr, addr2, self.mem.read_b(addr2));
            },
            Indirect => {
                let addr_lower = self.mem.read_b(self.pc);
                let addr_upper = self.mem.read_b(self.pc + 1);
                let addr = (addr_upper as u16) << 8 | addr_lower as u16;

                let lower = self.mem.read_b(addr);
                let addr2 = (addr & 0xff00) | ((addr + 1) & 0xff);
                let upper = self.mem.read_b(addr2);
                let addr_rel = (upper as u16) << 8 | lower as u16;
                operand = format!("{:02X} {:02X}", addr_lower, addr_upper);
                instr_operand = format!("(${:04X}) = {:04X}", addr, addr_rel);
            },
            IndexedIndirect => {
                let addr = self.mem.read_b(self.pc);
                let addr2 = addr + self.x;
                let addr_rel_lower = self.mem.read_b(addr2 as u16);
                let addr_rel_upper = self.mem.read_b((addr2 + 1) as u16);
                let addr_rel = (addr_rel_upper as u16) << 8 | addr_rel_lower as u16;
                operand = format!("{:02X}", addr);
                instr_operand = format!("(${:02X},X) @ {:02X} = {:04X} = {:02X}", addr, addr2, addr_rel, self.mem.read_b(addr_rel));
            },
            IndirectIndexed => {
                let addr = self.mem.read_b(self.pc);
                let addr_rel_lower = self.mem.read_b(addr as u16);
                let addr_rel_upper = self.mem.read_b((addr + 1) as u16);
                let rel_addr = (addr_rel_upper as u16) << 8 | addr_rel_lower as u16;
                let result = rel_addr + self.y as u16;
                operand = format!("{:02X}", addr);
                instr_operand = format!("(${:02X}),Y = {:04X} @ {:04X} = {:02X}", addr, rel_addr, result, self.mem.read_b(result));
            },
            Implicit => ()
        };
        trace!("{:04X}  {:02X} {:5}  {:3?} {:26}  {}", old_pc, opc, operand, opcode.oper, instr_operand, registers);
    }

    fn set_flag(&mut self, flag: usize, value: bool) {
        if value {
            self.flags |= (1 << flag) as u8;
        } else {
            self.flags &= !(1 << flag) as u8;
        }
    }
    fn get_flag(&self, flag: usize) -> bool {
        self.flags & (1 << flag) != 0
    }

    #[wrappit]
    fn fetch_b(&mut self) -> u8 {
        let value = self.mem.read_b(self.pc);
        self.pc += 1;
        value
    }
    fn fetch_w(&mut self) -> u16 {
        let lower = self.fetch_b() as u16;
        let upper = self.fetch_b() as u16;
        upper << 8 | lower
    }
    fn fetch_opcode(&mut self) -> &'static opcodes::Opcode {
        &opcodes::OPCODES[&self.fetch_b()]
    }

    #[wrappit]
    fn push_b(&mut self, value: u8) {
        self.mem.write_b(STACK_PAGE | self.s as u16, value);
        self.s -= 1;
    }
    #[wrappit]
    fn pop_b(&mut self) -> u8 {
        self.s += 1;
        self.mem.read_b(STACK_PAGE | self.s as u16)
    }
    fn push_w(&mut self, value: u16) {
        let upper = (value >> 8) as u8 & 0xff;
        let lower = (value & 0xff) as u8;
        self.push_b(upper);
        self.push_b(lower);
    }
    fn pop_w(&mut self) -> u16 {
        let lower = self.pop_b();
        let upper = self.pop_b();
        ((upper as u16) << 8) | lower as u16
    }

    fn decode(&mut self, opcode: &opcodes::Opcode)
    {
        use opcodes::Instruction::*;

        let function = match opcode.oper {
            ADC => Cpu::adc, 
            AND => Cpu::and, 
            ASL => Cpu::asl,
            BCC => Cpu::bcc,
            BCS => Cpu::bcs,
            BEQ => Cpu::beq,
            BIT => Cpu::bit,
            BMI => Cpu::bmi,
            BNE => Cpu::bne,
            BPL => Cpu::bpl,
            BRK => Cpu::brk,
            BVC => Cpu::bvc,
            BVS => Cpu::bvs,
            CLC => Cpu::clc,
            CLD => Cpu::cld, 
            CLI => Cpu::cli, 
            CLV => Cpu::clv, 
            CMP => Cpu::cmp,
            CPX => Cpu::cpx,
            CPY => Cpu::cpy,
            DEC => Cpu::dec,
            DEX => Cpu::dex, 
            DEY => Cpu::dey,
            EOR => Cpu::eor,
            INC => Cpu::inc,
            INX => Cpu::inx,
            INY => Cpu::iny,
            JMP => Cpu::jmp,
            JSR => Cpu::jsr,
            LDA => Cpu::lda,
            LDX => Cpu::ldx,
            LDY => Cpu::ldy,
            LSR => Cpu::lsr,
            NOP => Cpu::nop,
            ORA => Cpu::ora,
            PHA => Cpu::pha,
            PHP => Cpu::php,
            PLA => Cpu::pla,
            PLP => Cpu::plp,
            ROL => Cpu::rol,
            ROR => Cpu::ror,
            RTI => Cpu::rti,
            RTS => Cpu::rts,
            SBC => Cpu::sbc,
            SEC => Cpu::sec,
            SED => Cpu::sed,
            SEI => Cpu::sei,
            STA => Cpu::sta,
            STX => Cpu::stx,
            STY => Cpu::sty,
            TAX => Cpu::tax,
            TAY => Cpu::tay,
            TSX => Cpu::tsx,
            TXA => Cpu::txa,
            TXS => Cpu::txs,
            TYA => Cpu::tya,
            /* Illegal opcodes */
            LAX => Cpu::lax,
            SAX => Cpu::sax,
            DCP => Cpu::dcp,
            ISB => Cpu::isb,
            SLO => Cpu::slo,
            RLA => Cpu::rla,
            SRE => Cpu::sre,
            RRA => Cpu::rra,
        };
        function(self, &opcode.mode);
    }

    #[wrappit]
    fn fetch_mem_address(&mut self, mode: &opcodes::AddressingMode) -> (u16, bool) {
        use opcodes::AddressingMode::*;

        let mut page_crossed = false;
        let addr = match mode {
            ZeroPage => self.fetch_b().into(),
            ZeroPageX => (self.fetch_b() + self.x).into(),
            ZeroPageY => (self.fetch_b() + self.y).into(),
            Absolute => self.fetch_w(),
            AbsoluteX => {
                let fetched = self.fetch_w();
                let addr = fetched + self.x as u16;
                page_crossed = addr & 0xff00 != fetched & 0xff00;
                addr
            },
            AbsoluteY => {
                let fetched = self.fetch_w();
                let addr = fetched + self.y as u16;
                page_crossed = addr & 0xff00 != fetched & 0xff00;
                addr
            },
            Indirect => {
                let addr = self.fetch_w();
                let lower = self.mem.read_b(addr);
                let addr = (addr & 0xff00) | ((addr + 1) & 0xff);
                let upper = self.mem.read_b(addr);
                (upper as u16) << 8 | lower as u16
            },
            IndexedIndirect => {
                let addr = self.fetch_b() + self.x;
                let addr_rel_lower = self.mem.read_b(addr as u16);
                let addr_rel_upper = self.mem.read_b((addr + 1) as u16);
                (addr_rel_upper as u16) << 8 | addr_rel_lower as u16
            },
            IndirectIndexed => {
                let addr = self.fetch_b();
                let addr_rel_lower = self.mem.read_b(addr as u16);
                let addr_rel_upper = self.mem.read_b((addr + 1) as u16);
                let rel_addr = (addr_rel_upper as u16) << 8 | addr_rel_lower as u16;
                let result = rel_addr + self.y as u16;
                page_crossed = rel_addr & 0xff00 != result & 0xff00;
                result
            },
            unknown => {
                error!("Cannot get memory address for {:?}", unknown);
                panic!();
            }
        };
        (addr, page_crossed)
    }

    #[wrappit]
    fn fetch_with_mode(&mut self, mode: &opcodes::AddressingMode) ->  (i32, bool) {
        use opcodes::AddressingMode::{Acc, Immediate, Relative, Implicit};

        match mode {
            Acc => (self.a.into(), false),
            Immediate => (self.fetch_b().into(), false),
            Relative => ((self.fetch_b() as i8).into(), false), // To get negative relative jumps
            Implicit => (0x0123dead, false),
            other => {
                let (address, page_crossed) = self.fetch_mem_address(other);
                (self.mem.read_b(address).into(), page_crossed)
            }
        }
    }

    #[wrappit]
    fn do_on_mem(&mut self, mode: &opcodes::AddressingMode, fun: &dyn Fn(&mut Self, u8) -> u8) {
        use opcodes::AddressingMode::*;

        let addr = match mode {
            Acc => {
                self.a = fun(self, self.a);
                return;
            },
            other => self.fetch_mem_address(other).0
        };

        let value = self.mem.read_b(addr);
        let result = fun(self, value);
        self.mem.write_b(addr, result);
    }

    fn adc(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);
        let carry = self.get_flag(CARRY) as i32;
        let sum = self.a as i32 + value + carry;
        let a = self.a as i32;

        self.set_flag(ZERO, sum & 0xff == 0);
        self.set_flag(CARRY, sum > 0xff);
        self.set_flag(NEGATIVE, (sum as i8) < 0);
        self.set_flag(OVERFLOW, (a ^ value) & 0x80 == 0 && (a ^ sum) & 0x80 != 0);
        self.a = (sum & 0xff) as u8;
        self.cycles_to_go += page_crossed as u8;
    }

    fn and(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.a &= value as u8;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, (self.a as i8) < 0);
        self.cycles_to_go += page_crossed as u8;
    }

    fn asl(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::asl_impl);
    }
    fn asl_impl(&mut self, val1: u8) -> u8 {
        let carry = val1 & 0x80 != 0;
        let result = val1 << 1;

        self.set_flag(CARRY, carry);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        result
    }

    fn bcc(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if !self.get_flag(CARRY) {
            self.relative_branch(offset);
        }
    }
    fn bcs(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if self.get_flag(CARRY) {
            self.relative_branch(offset);
        }
    }
    fn beq(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if self.get_flag(ZERO) {
            self.relative_branch(offset);
        }
    }
    fn bne(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if !self.get_flag(ZERO) {
            self.relative_branch(offset);
        }
    }
    fn bmi(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if self.get_flag(NEGATIVE) {
            self.relative_branch(offset);
        }
    }
    fn bpl(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if !self.get_flag(NEGATIVE) {
            self.relative_branch(offset);
        }
    }
    fn bvc(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if !self.get_flag(OVERFLOW) {
            self.relative_branch(offset);
        }
    }
    fn bvs(&mut self, mode: &opcodes::AddressingMode) {
        let (offset, _) = self.fetch_with_mode(mode);
        if self.get_flag(OVERFLOW) {
            self.relative_branch(offset);
        }
    }
    fn relative_branch(&mut self, offset: i32) {
        let old_pc = self.pc;
        self.pc = (self.pc as i32 + offset) as u16;
        self.cycles_to_go += if old_pc & 0xff00 == self.pc & 0xff00 { 1 } else { 2 };
    }

    fn bit(&mut self, mode: &opcodes::AddressingMode) {
        let (value, _) = self.fetch_with_mode(mode);
        let result = self.a & value as u8;
        self.set_flag(ZERO, result == 0);
        self.set_flag(OVERFLOW, value & (1 << OVERFLOW) != 0);
        self.set_flag(NEGATIVE, value & (1 << NEGATIVE) != 0);
    }

    fn brk(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(B_FLAG_0, true);
        self.push_w(self.pc);
        self.push_b(self.flags | (1 << B_FLAG_1));
        self.set_flag(INT_DISABLE, true);
        self.pc = self.mem.read_w(IRQ_VECTOR);
    }

    fn clc(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(CARRY, false);
    }
    fn cld(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(DECIMAL, false);
    }
    fn cli(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(INT_DISABLE, false);
    }
    fn clv(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(OVERFLOW, false);
    }

    #[wrappit]
    fn cmp(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);
        let value = value as u8;

        self.set_flag(CARRY, self.a >= value);
        self.set_flag(ZERO, self.a == value);
        self.set_flag(NEGATIVE, (self.a - value) & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }
    #[wrappit]
    fn cpx(&mut self, mode: &opcodes::AddressingMode) {
        let (value, _) = self.fetch_with_mode(mode);
        let value = value as u8;

        self.set_flag(CARRY, self.x >= value);
        self.set_flag(ZERO, self.x == value);
        self.set_flag(NEGATIVE, (self.x - value) & 0x80 != 0);
    }
    #[wrappit]
    fn cpy(&mut self, mode: &opcodes::AddressingMode) {
        let (value, _) = self.fetch_with_mode(mode);
        let value = value as u8;

        self.set_flag(CARRY, self.y >= value);
        self.set_flag(ZERO, self.y == value);
        self.set_flag(NEGATIVE, (self.y - value) & 0x80 != 0);
    }

    fn dec(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::dec_impl);
    }
    #[wrappit]
    fn dec_impl(&mut self, value: u8) -> u8 {
        let result = value - 1;
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        result
    }
    #[wrappit]
    fn dex(&mut self, _: &opcodes::AddressingMode) {
        self.x -= 1;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
    }
    #[wrappit]
    fn dey(&mut self, _: &opcodes::AddressingMode) {
        self.y -= 1;
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
    }

    fn eor(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.a ^= value as u8;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }

    fn inc(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::inc_impl);
    }
    #[wrappit]
    fn inc_impl(&mut self, value: u8) -> u8 {
        let value = value + 1;
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);
        value
    }
    #[wrappit]
    fn inx(&mut self, _: &opcodes::AddressingMode) {
        self.x += 1;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
    }
    #[wrappit]
    fn iny(&mut self, _: &opcodes::AddressingMode) {
        self.y += 1;
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
    }

    #[wrappit]
    fn jmp(&mut self, mode: &opcodes::AddressingMode) {
        self.pc = self.fetch_mem_address(mode).0;
    }
    #[wrappit]
    fn jsr(&mut self, mode: &opcodes::AddressingMode) {
        let address = self.fetch_mem_address(mode).0;
        self.push_w(self.pc - 1);
        self.pc = address;
    }

    fn lda(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.a = value as u8;
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }
    fn ldx(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.x = value as u8;
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }
    fn ldy(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.y = value as u8;
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }

    fn lsr(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::lsr_impl);
    }
    fn lsr_impl(&mut self, value: u8) -> u8 {
        self.set_flag(CARRY, value & 0x01 != 0);

        let result = value >> 1;
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        result
    }

    fn nop(&mut self, mode: &opcodes::AddressingMode) { 
        let (_, page_crossed) = self.fetch_with_mode(mode);
        self.cycles_to_go += page_crossed as u8;
    }

    fn ora(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.a |= value as u8;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }

    fn pha(&mut self, _: &opcodes::AddressingMode) {
        self.push_b(self.a);
    }
    fn php(&mut self, _: &opcodes::AddressingMode) {
        self.push_b(self.flags | 0x30);
    }
    fn pla(&mut self, _: &opcodes::AddressingMode) {
        self.a = self.pop_b();
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
    }
    fn plp(&mut self, _: &opcodes::AddressingMode) {
        self.flags = self.pop_b() & !(1 << B_FLAG_0) | (1 << B_FLAG_1);
    }

    fn rol(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::rol_impl);
    }
    fn rol_impl(&mut self, value: u8) -> u8 {
        let carry = (value & 0x80) != 0;
        let result = (value << 1) | self.get_flag(CARRY) as u8;

        self.set_flag(CARRY, carry);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        result
    }
    fn ror(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::ror_impl);
    }
    fn ror_impl(&mut self, value: u8) -> u8 {
        let carry = (value & 0x01) != 0;
        let result = (value >> 1) | (self.get_flag(CARRY) as u8) << 7;

        self.set_flag(CARRY, carry);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        result
    }

    fn rti(&mut self, mode: &opcodes::AddressingMode) {
        self.plp(mode);
        self.pc = self.pop_w();
    }
    #[wrappit]
    fn rts(&mut self, _: &opcodes::AddressingMode) {
        self.pc = self.pop_w() + 1;
    }

    fn sbc(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);
        let a = self.a as i32;
        let result = a - value - 1 + self.get_flag(CARRY) as i32;
        
        self.set_flag(CARRY, result >= 0);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        self.set_flag(OVERFLOW, (a ^ value) & 0x80 != 0 && (a ^ result) & 0x80 != 0);

        self.a = result as u8;
        self.cycles_to_go += page_crossed as u8;
    }

    fn sec(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(CARRY, true);
    }
    fn sed(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(DECIMAL, true);
    }
    fn sei(&mut self, _: &opcodes::AddressingMode) {
        self.set_flag(INT_DISABLE, true);
    }

    fn sta(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::sta_impl);
    }
    fn sta_impl(&mut self, _: u8) -> u8 {
        self.a
    }
    fn stx(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::stx_impl)
    }
    fn stx_impl(&mut self, _: u8) -> u8 {
        self.x
    }
    fn sty(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::sty_impl)
    }
    fn sty_impl(&mut self, _: u8) -> u8 {
        self.y
    }

    fn tax(&mut self, _: &opcodes::AddressingMode) {
        self.x = self.a;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
    }
    fn tay(&mut self, _: &opcodes::AddressingMode) {
        self.y = self.a;
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
    }
    fn tsx(&mut self, _: &opcodes::AddressingMode) {
        self.x = self.s;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
    }
    fn txa(&mut self, _: &opcodes::AddressingMode) {
        self.a = self.x;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
    }
    fn txs(&mut self, _: &opcodes::AddressingMode) {
        self.s = self.x;
    }
    fn tya(&mut self, _: &opcodes::AddressingMode) {
        self.a = self.y;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
    }

    /* Illegal opcodes */
    fn lax(&mut self, mode: &opcodes::AddressingMode) {
        let (value, page_crossed) = self.fetch_with_mode(mode);

        self.a = value as u8;
        self.x = value as u8;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.cycles_to_go += page_crossed as u8;
    }

    fn sax(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::sax_impl);
    }
    fn sax_impl(&mut self, _: u8) -> u8 {
        self.a & self.x
    }

    fn dcp(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::dcp_impl);
    }
    #[wrappit]
    fn dcp_impl(&mut self, value: u8) -> u8 {
        let value = value - 1;

        self.set_flag(CARRY, self.a >= value);
        self.set_flag(ZERO, self.a == value);
        self.set_flag(NEGATIVE, (self.a - value) & 0x80 != 0);
        value
    }

    fn isb(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::isb_impl);
    }
    #[wrappit]
    fn isb_impl(&mut self, value: u8) -> u8 {
        let value = value + 1;

        let a = self.a as i32;
        let result = a - value as i32 - 1 + self.get_flag(CARRY) as i32;
        
        self.set_flag(CARRY, result >= 0);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);
        self.set_flag(OVERFLOW, (a ^ value as i32) & 0x80 != 0 && (a ^ result) & 0x80 != 0);

        self.a = result as u8;
        value
    }

    fn slo(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::slo_impl);
    }
    fn slo_impl(&mut self, value: u8) -> u8 {
        let result = self.asl_impl(value);

        self.a |= result;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        result
    }

    fn rla(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::rla_impl);
    }
    fn rla_impl(&mut self, value: u8) -> u8 {
        let result = self.rol_impl(value);

        self.a &= result;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        result
    }

    fn sre(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::sre_impl);
    }
    fn sre_impl(&mut self, value: u8) -> u8 {
        let result = self.lsr_impl(value);

        self.a ^= result;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        result
    }

    fn rra(&mut self, mode: &opcodes::AddressingMode) {
        self.do_on_mem(mode, &Cpu::rra_impl);
    }
    fn rra_impl(&mut self, value: u8) -> u8 {
        let result = self.ror_impl(value);

        let carry = self.get_flag(CARRY) as i32;
        let sum = self.a as i32 + result as i32 + carry;
        let a = self.a as i32;

        self.set_flag(ZERO, sum & 0xff == 0);
        self.set_flag(CARRY, sum > 0xff);
        self.set_flag(NEGATIVE, (sum as i8) < 0);
        self.set_flag(OVERFLOW, (a ^ result as i32) & 0x80 == 0 && (a ^ sum) & 0x80 != 0);
        self.a = (sum & 0xff) as u8;

        result
    }
}

#[cfg(test)]
mod test {
use super::*;

fn prepare(mem: Vec<u8>, addr: u16) -> Cpu {
    let mut vec = vec![0u8; 0x0400];
    let mut iter = vec.iter_mut().skip(addr as usize);
    for value in mem {
        if let Some(v) = &mut iter.next() {
            **v = value;
        }
    }
    let bus = Rc::new(RefCell::new(bus::Bus::new()));
    let mut cpu = Cpu::new(Box::new(vec), bus);
    cpu.pc = addr;
    cpu
}

#[test]
fn test_adc_imm() {
    let mut cpu = prepare(vec![0x69, 121], 0x200);
    cpu.a = 12;
    cpu.set_flag(CARRY, true);
    cpu.tick();
    cpu.tick();

    assert_eq!(cpu.cycles_to_go, 0);
    assert_eq!(cpu.a, 134);
    assert!(!cpu.get_flag(CARRY));
    assert!(cpu.get_flag(NEGATIVE));
    assert!(cpu.get_flag(OVERFLOW));
    assert!(!cpu.get_flag(ZERO));

    let mut cpu = prepare(vec![0x69, 0xff], 0x200);
    cpu.a = 1;
    cpu.set_flag(CARRY, false);
    cpu.tick();
    assert_eq!(cpu.cycles_to_go, 1);
    cpu.tick();

    assert_eq!(cpu.a, 0);
    assert!(cpu.get_flag(CARRY));
    assert!(!cpu.get_flag(NEGATIVE));
    assert!(cpu.get_flag(ZERO));
    assert!(!cpu.get_flag(OVERFLOW));
}

#[test]
fn test_adr_acc() {
    let mut cpu = prepare(vec![], 0x200);
    cpu.a = 123;
    let (val, page_crossed) = cpu.fetch_with_mode(&opcodes::AddressingMode::Acc);

    assert_eq!(val, 123);
    assert!(!page_crossed);
}

#[test]
fn test_adr_imm() {
    let mut cpu = prepare(vec![0xab], 0x199);
    let (val, page_crossed) = cpu.fetch_with_mode(&opcodes::AddressingMode::Immediate);

    assert_eq!(val, 0xab);
    assert!(!page_crossed);
}

#[test]
fn test_adr_zeropage() {
    let mut cpu = prepare(vec![0xab], 0x199);
    cpu.mem.write_b(0xab, 231);
    let (val, page_crossed) = cpu.fetch_with_mode(&opcodes::AddressingMode::ZeroPage);

    assert_eq!(val, 231);
    assert!(!page_crossed);
}

#[test]
fn test_adr_zeropage_x() {
    let mut cpu = prepare(vec![0xab], 0x199);
    cpu.mem.write_b(0xb0, 118);
    cpu.x = 5;
    let (val, page_crossed) = cpu.fetch_with_mode(&opcodes::AddressingMode::ZeroPageX);

    assert_eq!(val, 118);
    assert!(!page_crossed);

    let mut cpu = prepare(vec![0xf0], 0x199);
    cpu.mem.write_b(0x04, 44);
    cpu.x = 20;
    let (val, page_crossed) = cpu.fetch_with_mode(&opcodes::AddressingMode::ZeroPageX);

    assert_eq!(val, 44);
    assert!(!page_crossed);
}

#[test]
fn test_tax() {
    let mut cpu = prepare(vec![0xaa], 0x200);

    // cpu.mem.write_b(0x100, 0xaa);
    cpu.a = 0xab;
    cpu.tick();
    cpu.tick();
    assert_eq!(cpu.x, cpu.a);
}    
}