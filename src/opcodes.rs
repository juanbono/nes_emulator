use crate::cpu::AddressingMode;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, mnemonic: &'static str, len: u8, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            mnemonic,
            len,
            cycles,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        // Break instruction
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),

        // Register instructions
        // These instructions are implied mode, have a length of one byte and require two machine cycles.
        OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xA8, "TAY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x8A, "TXA", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xE8, "INX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xC8, "INY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xCA, "DEX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),

        // Flag (Processor Status) Instructions
        OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xB8, "CLV", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xD8, "CLD", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xF8, "SED", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(0xb9, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb1, "LDA", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X),
        OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
        OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),

        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3D, "AND", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(0x39, "AND", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x31, "AND", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };

}
