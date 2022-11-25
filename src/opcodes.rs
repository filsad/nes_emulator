use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    ZeroPageX, ZeroPageY, AbsoluteX, AbsoluteY, IndexedIndirect, IndirectIndexed, Implicit, Acc, Immediate, ZeroPage, Absolute, Relative, Indirect
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX,
    CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA,
    PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
    /* Illegal opcodes */
    LAX, SAX, DCP, ISB, SLO, RLA, SRE, RRA,
}

#[derive(Debug, Clone)]
pub struct Opcode {
    pub oper: Instruction,
    pub mode: AddressingMode,
    pub cycles: u8
}

lazy_static! {
    pub static ref OPCODES: HashMap<u8, Opcode> = {
        let mut m = HashMap::new();
        m.insert(0x69, Opcode { oper: Instruction::ADC, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x65, Opcode { oper: Instruction::ADC, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x75, Opcode { oper: Instruction::ADC, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x6d, Opcode { oper: Instruction::ADC, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0x7d, Opcode { oper: Instruction::ADC, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x79, Opcode { oper: Instruction::ADC, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0x61, Opcode { oper: Instruction::ADC, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x71, Opcode { oper: Instruction::ADC, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0x29, Opcode { oper: Instruction::AND, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x25, Opcode { oper: Instruction::AND, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x35, Opcode { oper: Instruction::AND, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x2d, Opcode { oper: Instruction::AND, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0x3d, Opcode { oper: Instruction::AND, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x39, Opcode { oper: Instruction::AND, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0x21, Opcode { oper: Instruction::AND, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x31, Opcode { oper: Instruction::AND, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0x0a, Opcode { oper: Instruction::ASL, mode: AddressingMode::Acc, cycles: 2});
        m.insert(0x06, Opcode { oper: Instruction::ASL, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x16, Opcode { oper: Instruction::ASL, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x0e, Opcode { oper: Instruction::ASL, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x1e, Opcode { oper: Instruction::ASL, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0x90, Opcode { oper: Instruction::BCC, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0xb0, Opcode { oper: Instruction::BCS, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0xf0, Opcode { oper: Instruction::BEQ, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0x30, Opcode { oper: Instruction::BMI, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0xd0, Opcode { oper: Instruction::BNE, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0x10, Opcode { oper: Instruction::BPL, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0x50, Opcode { oper: Instruction::BVC, mode: AddressingMode::Relative, cycles: 2});
        m.insert(0x70, Opcode { oper: Instruction::BVS, mode: AddressingMode::Relative, cycles: 2});

        m.insert(0x24, Opcode { oper: Instruction::BIT, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x2c, Opcode { oper: Instruction::BIT, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0x00, Opcode { oper: Instruction::BRK, mode: AddressingMode::Implicit, cycles: 7});

        m.insert(0x18, Opcode { oper: Instruction::CLC, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xd8, Opcode { oper: Instruction::CLD, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x58, Opcode { oper: Instruction::CLI, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xb8, Opcode { oper: Instruction::CLV, mode: AddressingMode::Implicit, cycles: 2});

        m.insert(0xc9, Opcode { oper: Instruction::CMP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xc5, Opcode { oper: Instruction::CMP, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xd5, Opcode { oper: Instruction::CMP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xcd, Opcode { oper: Instruction::CMP, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xdd, Opcode { oper: Instruction::CMP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0xd9, Opcode { oper: Instruction::CMP, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0xc1, Opcode { oper: Instruction::CMP, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0xd1, Opcode { oper: Instruction::CMP, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0xe0, Opcode { oper: Instruction::CPX, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xe4, Opcode { oper: Instruction::CPX, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xec, Opcode { oper: Instruction::CPX, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0xc0, Opcode { oper: Instruction::CPY, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xc4, Opcode { oper: Instruction::CPY, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xcc, Opcode { oper: Instruction::CPY, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0xc6, Opcode { oper: Instruction::DEC, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0xd6, Opcode { oper: Instruction::DEC, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0xce, Opcode { oper: Instruction::DEC, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0xde, Opcode { oper: Instruction::DEC, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0xca, Opcode { oper: Instruction::DEX, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x88, Opcode { oper: Instruction::DEY, mode: AddressingMode::Implicit, cycles: 2});

        m.insert(0x49, Opcode { oper: Instruction::EOR, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x45, Opcode { oper: Instruction::EOR, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x55, Opcode { oper: Instruction::EOR, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x4d, Opcode { oper: Instruction::EOR, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0x5d, Opcode { oper: Instruction::EOR, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x59, Opcode { oper: Instruction::EOR, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0x41, Opcode { oper: Instruction::EOR, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x51, Opcode { oper: Instruction::EOR, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0xe6, Opcode { oper: Instruction::INC, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0xf6, Opcode { oper: Instruction::INC, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0xee, Opcode { oper: Instruction::INC, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0xfe, Opcode { oper: Instruction::INC, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0xe8, Opcode { oper: Instruction::INX, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xc8, Opcode { oper: Instruction::INY, mode: AddressingMode::Implicit, cycles: 2});

        m.insert(0x4c, Opcode { oper: Instruction::JMP, mode: AddressingMode::Absolute, cycles: 3});
        m.insert(0x6c, Opcode { oper: Instruction::JMP, mode: AddressingMode::Indirect, cycles: 5});

        m.insert(0x20, Opcode { oper: Instruction::JSR, mode: AddressingMode::Absolute, cycles: 6});

        m.insert(0xa9, Opcode { oper: Instruction::LDA, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xa5, Opcode { oper: Instruction::LDA, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xb5, Opcode { oper: Instruction::LDA, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xad, Opcode { oper: Instruction::LDA, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xbd, Opcode { oper: Instruction::LDA, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0xb9, Opcode { oper: Instruction::LDA, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0xa1, Opcode { oper: Instruction::LDA, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0xb1, Opcode { oper: Instruction::LDA, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0xa2, Opcode { oper: Instruction::LDX, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xa6, Opcode { oper: Instruction::LDX, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xb6, Opcode { oper: Instruction::LDX, mode: AddressingMode::ZeroPageY, cycles: 4});
        m.insert(0xae, Opcode { oper: Instruction::LDX, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xbe, Opcode { oper: Instruction::LDX, mode: AddressingMode::AbsoluteY, cycles: 4});

        m.insert(0xa0, Opcode { oper: Instruction::LDY, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xa4, Opcode { oper: Instruction::LDY, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xb4, Opcode { oper: Instruction::LDY, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xac, Opcode { oper: Instruction::LDY, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xbc, Opcode { oper: Instruction::LDY, mode: AddressingMode::AbsoluteX, cycles: 4});

        m.insert(0x4a, Opcode { oper: Instruction::LSR, mode: AddressingMode::Acc, cycles: 2});
        m.insert(0x46, Opcode { oper: Instruction::LSR, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x56, Opcode { oper: Instruction::LSR, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x4e, Opcode { oper: Instruction::LSR, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x5e, Opcode { oper: Instruction::LSR, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0xea, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});

        m.insert(0x09, Opcode { oper: Instruction::ORA, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x05, Opcode { oper: Instruction::ORA, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x15, Opcode { oper: Instruction::ORA, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x0d, Opcode { oper: Instruction::ORA, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0x1d, Opcode { oper: Instruction::ORA, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x19, Opcode { oper: Instruction::ORA, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0x01, Opcode { oper: Instruction::ORA, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x11, Opcode { oper: Instruction::ORA, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0x48, Opcode { oper: Instruction::PHA, mode: AddressingMode::Implicit, cycles: 3});
        m.insert(0x08, Opcode { oper: Instruction::PHP, mode: AddressingMode::Implicit, cycles: 3});
        m.insert(0x68, Opcode { oper: Instruction::PLA, mode: AddressingMode::Implicit, cycles: 4});
        m.insert(0x28, Opcode { oper: Instruction::PLP, mode: AddressingMode::Implicit, cycles: 4});

        m.insert(0x2a, Opcode { oper: Instruction::ROL, mode: AddressingMode::Acc, cycles: 2});
        m.insert(0x26, Opcode { oper: Instruction::ROL, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x36, Opcode { oper: Instruction::ROL, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x2e, Opcode { oper: Instruction::ROL, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x3e, Opcode { oper: Instruction::ROL, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0x6a, Opcode { oper: Instruction::ROR, mode: AddressingMode::Acc, cycles: 2});
        m.insert(0x66, Opcode { oper: Instruction::ROR, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x76, Opcode { oper: Instruction::ROR, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x6e, Opcode { oper: Instruction::ROR, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x7e, Opcode { oper: Instruction::ROR, mode: AddressingMode::AbsoluteX, cycles: 7});

        m.insert(0x40, Opcode { oper: Instruction::RTI, mode: AddressingMode::Implicit, cycles: 6});
        m.insert(0x60, Opcode { oper: Instruction::RTS, mode: AddressingMode::Implicit, cycles: 6});

        m.insert(0xe9, Opcode { oper: Instruction::SBC, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xe5, Opcode { oper: Instruction::SBC, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xf5, Opcode { oper: Instruction::SBC, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xed, Opcode { oper: Instruction::SBC, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xfd, Opcode { oper: Instruction::SBC, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0xf9, Opcode { oper: Instruction::SBC, mode: AddressingMode::AbsoluteY, cycles: 4});
        m.insert(0xe1, Opcode { oper: Instruction::SBC, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0xf1, Opcode { oper: Instruction::SBC, mode: AddressingMode::IndirectIndexed, cycles: 5});

        m.insert(0x38, Opcode { oper: Instruction::SEC, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xf8, Opcode { oper: Instruction::SED, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x78, Opcode { oper: Instruction::SEI, mode: AddressingMode::Implicit, cycles: 2});

        m.insert(0x85, Opcode { oper: Instruction::STA, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x95, Opcode { oper: Instruction::STA, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x8d, Opcode { oper: Instruction::STA, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0x9d, Opcode { oper: Instruction::STA, mode: AddressingMode::AbsoluteX, cycles: 5});
        m.insert(0x99, Opcode { oper: Instruction::STA, mode: AddressingMode::AbsoluteY, cycles: 5});
        m.insert(0x81, Opcode { oper: Instruction::STA, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x91, Opcode { oper: Instruction::STA, mode: AddressingMode::IndirectIndexed, cycles: 6});

        m.insert(0x86, Opcode { oper: Instruction::STX, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x96, Opcode { oper: Instruction::STX, mode: AddressingMode::ZeroPageY, cycles: 4});
        m.insert(0x8e, Opcode { oper: Instruction::STX, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0x84, Opcode { oper: Instruction::STY, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x94, Opcode { oper: Instruction::STY, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x8c, Opcode { oper: Instruction::STY, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0xaa, Opcode { oper: Instruction::TAX, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xa8, Opcode { oper: Instruction::TAY, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xba, Opcode { oper: Instruction::TSX, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x8a, Opcode { oper: Instruction::TXA, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x9a, Opcode { oper: Instruction::TXS, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x98, Opcode { oper: Instruction::TYA, mode: AddressingMode::Implicit, cycles: 2});

        // Illegal opcodes:
        m.insert(0xa7, Opcode { oper: Instruction::LAX, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0xb7, Opcode { oper: Instruction::LAX, mode: AddressingMode::ZeroPageY, cycles: 4});
        m.insert(0xa3, Opcode { oper: Instruction::LAX, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0xb3, Opcode { oper: Instruction::LAX, mode: AddressingMode::IndirectIndexed, cycles: 5});
        m.insert(0xaf, Opcode { oper: Instruction::LAX, mode: AddressingMode::Absolute, cycles: 4});
        m.insert(0xbf, Opcode { oper: Instruction::LAX, mode: AddressingMode::AbsoluteY, cycles: 4});

        m.insert(0x87, Opcode { oper: Instruction::SAX, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x97, Opcode { oper: Instruction::SAX, mode: AddressingMode::ZeroPageY, cycles: 4});
        m.insert(0x83, Opcode { oper: Instruction::SAX, mode: AddressingMode::IndexedIndirect, cycles: 6});
        m.insert(0x8f, Opcode { oper: Instruction::SAX, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0xeb, Opcode { oper: Instruction::SBC, mode: AddressingMode::Immediate, cycles: 2});

        m.insert(0xc7, Opcode { oper: Instruction::DCP, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0xd7, Opcode { oper: Instruction::DCP, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0xc3, Opcode { oper: Instruction::DCP, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0xd3, Opcode { oper: Instruction::DCP, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0xcf, Opcode { oper: Instruction::DCP, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0xdf, Opcode { oper: Instruction::DCP, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0xdb, Opcode { oper: Instruction::DCP, mode: AddressingMode::AbsoluteY, cycles: 7});

        m.insert(0xe7, Opcode { oper: Instruction::ISB, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0xf7, Opcode { oper: Instruction::ISB, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0xe3, Opcode { oper: Instruction::ISB, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0xf3, Opcode { oper: Instruction::ISB, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0xef, Opcode { oper: Instruction::ISB, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0xff, Opcode { oper: Instruction::ISB, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0xfb, Opcode { oper: Instruction::ISB, mode: AddressingMode::AbsoluteY, cycles: 7});

        m.insert(0x07, Opcode { oper: Instruction::SLO, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x17, Opcode { oper: Instruction::SLO, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x03, Opcode { oper: Instruction::SLO, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0x13, Opcode { oper: Instruction::SLO, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0x0f, Opcode { oper: Instruction::SLO, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x1f, Opcode { oper: Instruction::SLO, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0x1b, Opcode { oper: Instruction::SLO, mode: AddressingMode::AbsoluteY, cycles: 7});

        m.insert(0x27, Opcode { oper: Instruction::RLA, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x37, Opcode { oper: Instruction::RLA, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x23, Opcode { oper: Instruction::RLA, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0x33, Opcode { oper: Instruction::RLA, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0x2f, Opcode { oper: Instruction::RLA, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x3f, Opcode { oper: Instruction::RLA, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0x3b, Opcode { oper: Instruction::RLA, mode: AddressingMode::AbsoluteY, cycles: 7});

        m.insert(0x47, Opcode { oper: Instruction::SRE, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x57, Opcode { oper: Instruction::SRE, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x43, Opcode { oper: Instruction::SRE, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0x53, Opcode { oper: Instruction::SRE, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0x4f, Opcode { oper: Instruction::SRE, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x5f, Opcode { oper: Instruction::SRE, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0x5b, Opcode { oper: Instruction::SRE, mode: AddressingMode::AbsoluteY, cycles: 7});

        m.insert(0x67, Opcode { oper: Instruction::RRA, mode: AddressingMode::ZeroPage, cycles: 5});
        m.insert(0x77, Opcode { oper: Instruction::RRA, mode: AddressingMode::ZeroPageX, cycles: 6});
        m.insert(0x63, Opcode { oper: Instruction::RRA, mode: AddressingMode::IndexedIndirect, cycles: 8});
        m.insert(0x73, Opcode { oper: Instruction::RRA, mode: AddressingMode::IndirectIndexed, cycles: 8});
        m.insert(0x6f, Opcode { oper: Instruction::RRA, mode: AddressingMode::Absolute, cycles: 6});
        m.insert(0x7f, Opcode { oper: Instruction::RRA, mode: AddressingMode::AbsoluteX, cycles: 7});
        m.insert(0x7b, Opcode { oper: Instruction::RRA, mode: AddressingMode::AbsoluteY, cycles: 7});






        m.insert(0x0b, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x2b, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x4b, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x6b, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x8b, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xab, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xcb, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});

        m.insert(0x80, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x82, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0x89, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xc2, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});
        m.insert(0xe2, Opcode { oper: Instruction::NOP, mode: AddressingMode::Immediate, cycles: 2});

        m.insert(0x04, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x44, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPage, cycles: 3});
        m.insert(0x64, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPage, cycles: 3});

        m.insert(0x14, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x34, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x54, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0x74, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xd4, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});
        m.insert(0xf4, Opcode { oper: Instruction::NOP, mode: AddressingMode::ZeroPageX, cycles: 4});

        m.insert(0x0c, Opcode { oper: Instruction::NOP, mode: AddressingMode::Absolute, cycles: 4});

        m.insert(0x9c, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 5});

        m.insert(0x1c, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x3c, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x5c, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0x7c, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0xdc, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});
        m.insert(0xfc, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteX, cycles: 4});

        m.insert(0x9f, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteY, cycles: 5});
        m.insert(0x9e, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteY, cycles: 5});
        m.insert(0x9b, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteY, cycles: 5});
        m.insert(0xbb, Opcode { oper: Instruction::NOP, mode: AddressingMode::AbsoluteY, cycles: 5});

        m.insert(0x1a, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x3a, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x5a, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0x7a, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xda, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});
        m.insert(0xfa, Opcode { oper: Instruction::NOP, mode: AddressingMode::Implicit, cycles: 2});

        m
    };
}
