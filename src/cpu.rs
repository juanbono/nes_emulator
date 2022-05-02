// Recursos disponibles para el CPU:
// 1. Memory Map
// 2. CPU Registers
use crate::opcodes;
use flagset::{flags, FlagSet};
use std::collections::HashMap;

flags! {
    pub enum StatusFlag: u8 {
        Carry = 0b0000_0001,
        Zero = 0b0000_0010,
        InterruptDisable = 0b0000_0100,
        DecimalMode = 0b0000_1000,
        Break = 0b0001_0000,
        Break2 = 0b0010_0000,
        Overflow = 0b0100_0000,
        Negative = 0b1000_0000,
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

/// The NES CPU.
/// It uses Little-Endian addressing rather than Big-Endian.
/// That means that the 8 least significant bits of an address
/// will be stored before the 8 most significant bits.
#[derive(Debug)]
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: FlagSet<StatusFlag>,
    pub program_counter: u16,
    memory: [u8; 65535], // 0xFFFF = all the CPU memory
}

impl CPU {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                pos.wrapping_add(self.register_x) as u16
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                pos.wrapping_add(self.register_y) as u16
            }
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::Absolute_X => {
                let base = self.mem_read(self.program_counter);
                base.wrapping_add(self.register_x) as u16
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read(self.program_counter);
                base.wrapping_add(self.register_y) as u16
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);

                deref_base.wrapping_add(self.register_y as u16)
            }
            AddressingMode::NoneAddressing => panic!("mode {:?} is not supported.", mode),
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn run(&mut self) {
        let opcodes: &HashMap<u8, &'static opcodes::OpCode> = &(*opcodes::OPCODES_MAP);

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .unwrap_or_else(|| panic!("OpCode {:x} is not recognized", code));

            match code {
                /* LDA */
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }
                /* LDX */
                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
                    self.ldx(&opcode.mode);
                }
                /* LDY */
                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
                    self.ldy(&opcode.mode);
                }
                /* STA */
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }
                /* AND */
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),
                /* TAX */
                0xAA => self.tax(),
                /* TAY */
                0xA8 => self.tay(),
                /* TXA */
                0x8A => self.txa(),
                /* TYA */
                0x98 => self.tya(),
                /* INX */
                0xe8 => self.inx(),
                /* INY */
                0xC8 => self.iny(),
                /* DEX */
                0xCA => self.dex(),
                /* DEY */
                0x88 => self.dey(),
                /* CLC */
                0x18 => self.clc(),
                /* SEC */
                0x38 => self.sec(),
                /* CLI */
                0x58 => self.cli(),
                /* SEI */
                0x78 => self.sei(),
                /* CLV */
                0xB8 => self.clv(),
                /* CLD */
                0xD8 => self.cld(),
                /* SED */
                0xF8 => self.sed(),
                /* BRK */
                0x00 => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    /// Restores the state of all registers and initialize program_counter by
    /// the 2-byte value stored at 0xFFFC
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status.clear();

        self.program_counter = self.mem_read_u16(0xFFFC)
    }

    /// Loads a program into PRG ROM space and save the reference to the
    /// code into 0xFFFC memory cell.
    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    ////////////////
    // Instructions
    ////////////////
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x)
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y)
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x)
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y)
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// clear carry flag
    fn clc(&mut self) {
        self.status &= !FlagSet::from(StatusFlag::Carry);
    }

    /// set carry flag
    fn sec(&mut self) {
        self.status |= StatusFlag::Carry;
    }

    /// clear interrupt flag
    fn cli(&mut self) {
        self.status &= !FlagSet::from(StatusFlag::InterruptDisable);
    }

    /// set interrupt flag
    fn sei(&mut self) {
        self.status |= StatusFlag::InterruptDisable;
    }

    /// clear overflow flag
    fn clv(&mut self) {
        self.status &= !FlagSet::from(StatusFlag::Overflow);
    }

    /// clear decimal mode flag
    fn cld(&mut self) {
        self.status &= !FlagSet::from(StatusFlag::DecimalMode);
    }

    /// set decimal mode flag
    fn sed(&mut self) {
        self.status |= StatusFlag::DecimalMode;
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            // perform bitwise OR
            self.status |= StatusFlag::Zero;
        } else {
            // perform bitwise AND
            self.status &= !FlagSet::from(StatusFlag::Zero);
        }

        if result & FlagSet::from(StatusFlag::Negative).bits() != 0 {
            self.status |= StatusFlag::Negative;
        } else {
            self.status &= !FlagSet::from(StatusFlag::Negative);
        }
    }
}

impl Default for CPU {
    fn default() -> CPU {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: None.into(),
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(StatusFlag::Zero));
        assert!(!cpu.status.contains(StatusFlag::Negative));
    }

    #[test]
    fn test_ldx_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA2, 0x05, 0x00]);

        assert_eq!(cpu.register_x, 0x05);
        assert!(!cpu.status.contains(StatusFlag::Zero));
        assert!(!cpu.status.contains(StatusFlag::Negative));
    }

    #[test]
    fn test_ldy_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA0, 0x05, 0x00]);

        assert_eq!(cpu.register_y, 0x05);
        assert!(!cpu.status.contains(StatusFlag::Zero));
        assert!(!cpu.status.contains(StatusFlag::Negative));
    }

    #[test]
    fn test_immediate_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);

        assert!(cpu.status.contains(StatusFlag::Zero));
    }

    #[test]
    fn test_immediate_ldx_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA2, 0x00, 0x00]);

        assert!(cpu.status.contains(StatusFlag::Zero));
    }

    #[test]
    fn test_immediate_ldy_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA0, 0x00, 0x00]);

        assert!(cpu.status.contains(StatusFlag::Zero));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.load_and_run(vec![0xA9, 0x0A, 0xAA, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        // the following program adds 0xFF to the register A then
        // moves the contents of register A to register X and then
        // increments by 1 two times the register X (leading to an overflow).
        let program = vec![0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00];
        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_iny_overflow() {
        let mut cpu = CPU::new();
        // the following program adds 0xFF to the register A then
        // moves the contents of register A to register Y and then
        // increments by 1 two times the register Y (leading to an overflow).
        let program = vec![0xA9, 0xFF, 0xA8, 0xC8, 0xC8, 0x00];
        cpu.load_and_run(program);

        assert_eq!(cpu.register_y, 1)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_ldx_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xA6, 0x10, 0x00]);

        assert_eq!(cpu.register_x, 0x55);
    }

    #[test]
    fn test_ldy_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xA4, 0x10, 0x00]);

        assert_eq!(cpu.register_y, 0x55);
    }

    #[test]
    fn test_immediate_add_operation() {
        let mut cpu = CPU::new();
        // the program loads 0b0000_0011 into the register A then
        // perform an AND with 0b0000_1111.
        let program = vec![0xa9, 0b0000_0011, 0x29, 0b0000_1111, 0x00];
        cpu.load_and_run(program);

        assert_eq!(0b0000_0011 & 0b0000_1111, cpu.register_a);
    }

    #[test]
    fn test_clc_clears_carry_flag() {
        let mut cpu = CPU::new();
        let program = vec![0x38, 0x18, 0x00];
        cpu.load_and_run(program);

        assert!(!cpu.status.contains(StatusFlag::Carry));
    }

    #[test]
    fn test_sec_sets_carry_flag() {
        let mut cpu = CPU::new();
        let program = vec![0x38, 0x00];
        cpu.load_and_run(program);

        assert!(cpu.status.contains(StatusFlag::Carry));
    }

    #[test]
    fn test_cli_clears_interrupt_flag() {
        let mut cpu = CPU::new();
        let program = vec![0x78, 0x58, 0x00];
        cpu.load_and_run(program);

        assert!(!cpu.status.contains(StatusFlag::InterruptDisable));
    }

    #[test]
    fn test_sei_sets_interrupt_flag() {
        let mut cpu = CPU::new();
        let program = vec![0x78, 0x00];
        cpu.load_and_run(program);

        assert!(cpu.status.contains(StatusFlag::InterruptDisable));
    }

    #[test]
    fn test_clv_clears_overflow_flag() {
        todo!()
    }

    #[test]
    fn test_cld_clears_decimal_mode_flag() {
        let mut cpu = CPU::new();
        let program = vec![0xF8, 0xD8, 0x00];
        cpu.load_and_run(program);

        assert!(!cpu.status.contains(StatusFlag::DecimalMode));
    }

    #[test]
    fn test_sed_sets_decimal_mode_flag() {
        let mut cpu = CPU::new();
        let program = vec![0xF8, 0x00];
        cpu.load_and_run(program);

        assert!(cpu.status.contains(StatusFlag::DecimalMode));
    }
}
