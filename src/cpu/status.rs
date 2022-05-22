use flagset::{flags, FlagSet};
use std::ops::{Deref, DerefMut};

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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Status(FlagSet<StatusFlag>);

impl Status {
    pub fn from(bits: u8) -> Self {
        let flags = FlagSet::<StatusFlag>::new_truncated(bits);
        Status(flags)
    }

    pub fn insert(&mut self, flag: StatusFlag) {
        self.0 |= flag
    }

    pub fn remove(&mut self, flag: StatusFlag) {
        self.0 &= !FlagSet::from(flag);
    }
}

impl Default for Status {
    fn default() -> Self {
        Status::from(0b0)
    }
}

impl Deref for Status {
    type Target = FlagSet<StatusFlag>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Status {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const FLAGS: [StatusFlag; 8] = [
        StatusFlag::Carry,
        StatusFlag::Zero,
        StatusFlag::InterruptDisable,
        StatusFlag::DecimalMode,
        StatusFlag::Break,
        StatusFlag::Break2,
        StatusFlag::Overflow,
        StatusFlag::Negative,
    ];

    #[test]
    fn default_status_is_empty() {
        let status = Status::default();

        assert!(FLAGS.into_iter().all(|flag| !status.contains(flag)))
    }
    #[test]
    fn can_be_created_from_bits() {
        let all_set = Status::from(0b1111_1111);

        assert!(FLAGS.into_iter().all(|flag| all_set.contains(flag)))
    }

    #[test]
    fn flags_can_be_set() {
        let mut status = Status::default();
        status.insert(StatusFlag::Carry);
        status.insert(StatusFlag::Negative);

        assert!(status.contains(StatusFlag::Carry));
        assert!(status.contains(StatusFlag::Negative));
    }

    #[test]
    fn flags_can_be_removed() {
        let mut status = Status::from(0b0000_0011);
        status.remove(StatusFlag::Carry);
        assert!(status.contains(StatusFlag::Zero));
        assert!(!status.contains(StatusFlag::Carry));
    }
}
