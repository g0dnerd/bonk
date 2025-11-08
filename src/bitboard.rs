use std::ops::*;

use crate::Square;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Bitboard(pub u64);

macro_rules! bb_impl_math_ops {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait for Bitboard {
            type Output = Self;

            fn $fn(self, rhs: Self) -> Self::Output {
                Self($trait::$fn(self.0, rhs.0))
            }
        }
    )*};
}

bb_impl_math_ops! {
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor;
}

macro_rules! bb_impl_math_assign_ops {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait for Bitboard {
            fn $fn(&mut self, rhs: Self) {
                $trait::$fn(&mut self.0, rhs.0)
            }
        }
    )*};
}

bb_impl_math_assign_ops! {
    BitAndAssign, bitand_assign;
    BitOrAssign, bitor_assign;
    BitXorAssign, bitxor_assign;
}

macro_rules! bb_impl_math_assign_ops_square {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait<Square> for Bitboard {
            #[allow(clippy::suspicious_op_assign_impl)]
            fn $fn(&mut self, rhs: Square) {
                $trait::$fn(&mut self.0, Self(1 << rhs).0)
            }
        }
    )*};
}

bb_impl_math_assign_ops_square! {
    BitAndAssign, bitand_assign;
    BitOrAssign, bitor_assign;
    BitXorAssign, bitxor_assign;
}

macro_rules! bb_impl_shift_ops {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait<usize> for Bitboard {
            type Output = Self;

            fn $fn(self, rhs: usize) -> Self::Output {
                Self(self.0.$fn(rhs as u32))
            }
        }
    )*};
}

bb_impl_shift_ops! {
    Shl, shl;
    Shr, shr;
}

impl Not for Bitboard {
    type Output = Bitboard;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl Bitboard {
    pub fn from_square(s: Square) -> Self {
        Self(1 << s)
    }

    pub fn colorflip(&self) -> Self {
        let mut flipped_self = Self(0);
        flipped_self.0 = self.0.swap_bytes();
        flipped_self
    }

    pub fn contains(&self, s: Square) -> bool {
        self.0 & 1 << s != 0
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn count_ones(&self) -> u32 {
        self.0.count_ones()
    }

    pub fn trailing_zeros(&self) -> u8 {
        self.0.trailing_zeros() as u8
    }

    pub fn clear_lsb(&mut self) {
        self.0 &= self.0 - 1;
    }
}

impl Iterator for Bitboard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            let ret = self.trailing_zeros();
            self.clear_lsb();
            Some(ret)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Squares;

    use super::*;

    #[test]
    fn bitboard_square_bitwise_sanity() {
        let mut bb = Bitboard(0);
        bb |= Squares::E2;
        assert_eq!(bb, Bitboard::from_square(Squares::E2));

        bb = Bitboard(0);
        bb &= Squares::E2;
        assert_eq!(bb, Bitboard(0));

        bb = Bitboard::from_square(Squares::D2);
        bb |= Squares::E2;
        bb ^= Squares::D2;
        assert_eq!(bb, Bitboard::from_square(Squares::E2));
    }

    #[test]
    fn bitboard_colorflip_sanity() {
        let bb = Bitboard(0xaa55);
        let flipped = bb.colorflip();
        assert_eq!(flipped, Bitboard(0x55aa000000000000));
    }
}
