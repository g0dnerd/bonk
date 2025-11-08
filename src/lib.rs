use once_cell::sync::Lazy;
use rand::{RngCore, rng};

use crate::bitboard::Bitboard;

pub mod bitboard;
pub mod evaluation;
pub mod movegen;
pub mod search;
pub mod state;

pub type Square = u8;
pub type CastlingRights = u8;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Move {
    pub start: Square,
    pub end: Square,
}

pub const PIECE_REPR: [[&str; 6]; 2] = [
    ["P", "N", "B", "R", "Q", "K"],
    ["p", "n", "b", "r", "q", "k"],
];

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Color {
    WHITE = 0,
    BLACK = 1,
}

impl Color {
    pub fn from_usize(value: usize) -> Self {
        match value {
            0 => Self::WHITE,
            1 => Self::BLACK,
            _ => unreachable!(),
        }
    }
}

impl std::ops::Not for Color {
    type Output = Color;

    fn not(self) -> Self::Output {
        match self {
            Color::WHITE => Color::BLACK,
            Color::BLACK => Color::WHITE,
        }
    }
}

impl std::ops::Not for &Color {
    type Output = Color;

    fn not(self) -> Self::Output {
        match self {
            Color::WHITE => Color::BLACK,
            Color::BLACK => Color::WHITE,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Piece {
    PAWN = 0,
    KNIGHT = 1,
    BISHOP = 2,
    ROOK = 3,
    QUEEN = 4,
    KING = 5,
}

impl Piece {
    pub fn from_usize(value: impl Into<usize>) -> Self {
        match value.into() {
            0 => Self::PAWN,
            1 => Self::KNIGHT,
            2 => Self::BISHOP,
            3 => Self::ROOK,
            4 => Self::QUEEN,
            5 => Self::KING,
            _ => unreachable!(),
        }
    }
}

pub struct Castling;

impl Castling {
    pub const NO_LEGAL: u8 = 0;
    pub const WHITE_KINGSIDE: u8 = 1;
    pub const WHITE_QUEENSIDE: u8 = 2;
    pub const BLACK_QUEENSIDE: u8 = 4;
    pub const BLACK_KINGSIDE: u8 = 8;
    pub const BOTH_KINGSIDES: u8 = Self::WHITE_KINGSIDE | Self::BLACK_KINGSIDE;
    pub const BOTH_QUEENSIDES: u8 = Self::WHITE_QUEENSIDE | Self::BLACK_QUEENSIDE;
    pub const WHITE_CASTLING: u8 = Self::WHITE_KINGSIDE | Self::WHITE_QUEENSIDE;
    pub const BLACK_CASTLING: u8 = Self::BLACK_KINGSIDE | Self::BLACK_QUEENSIDE;
    pub const ALL_LEGAL: u8 = Self::WHITE_CASTLING | Self::BLACK_CASTLING;
}

pub fn try_square_offset(s: Square, dx: i8, dy: i8) -> Option<Square> {
    let file = s % 8;
    let rank = s / 8;
    let new_file = file as i8 + dx;
    let new_rank = rank as i8 + dy;

    if (0..8).contains(&new_file) && (0..8).contains(&new_rank) {
        Some((new_rank * 8 + new_file) as Square)
    } else {
        None
    }
}

pub struct Squares;

impl Squares {
    pub const A1: Square = 0;
    pub const B1: Square = 1;
    pub const C1: Square = 2;
    pub const D1: Square = 3;
    pub const E1: Square = 4;
    pub const F1: Square = 5;
    pub const G1: Square = 6;
    pub const H1: Square = 7;
    pub const A2: Square = 8;
    pub const B2: Square = 9;
    pub const C2: Square = 10;
    pub const D2: Square = 11;
    pub const E2: Square = 12;
    pub const F2: Square = 13;
    pub const G2: Square = 14;
    pub const H2: Square = 15;
    pub const A3: Square = 16;
    pub const B3: Square = 17;
    pub const C3: Square = 18;
    pub const D3: Square = 19;
    pub const E3: Square = 20;
    pub const F3: Square = 21;
    pub const G3: Square = 22;
    pub const H3: Square = 23;
    pub const A4: Square = 24;
    pub const B4: Square = 25;
    pub const C4: Square = 26;
    pub const D4: Square = 27;
    pub const E4: Square = 28;
    pub const F4: Square = 29;
    pub const G4: Square = 30;
    pub const H4: Square = 31;
    pub const A5: Square = 32;
    pub const B5: Square = 33;
    pub const C5: Square = 34;
    pub const D5: Square = 35;
    pub const E5: Square = 36;
    pub const F5: Square = 37;
    pub const G5: Square = 38;
    pub const H5: Square = 39;
    pub const A6: Square = 40;
    pub const B6: Square = 41;
    pub const C6: Square = 42;
    pub const D6: Square = 43;
    pub const E6: Square = 44;
    pub const F6: Square = 45;
    pub const G6: Square = 46;
    pub const H6: Square = 47;
    pub const A7: Square = 48;
    pub const B7: Square = 49;
    pub const C7: Square = 50;
    pub const D7: Square = 51;
    pub const E7: Square = 52;
    pub const F7: Square = 53;
    pub const G7: Square = 54;
    pub const H7: Square = 55;
    pub const A8: Square = 56;
    pub const B8: Square = 57;
    pub const C8: Square = 58;
    pub const D8: Square = 59;
    pub const E8: Square = 60;
    pub const F8: Square = 61;
    pub const G8: Square = 62;
    pub const H8: Square = 63;
}

pub static ZOBRIST_KEYS: Lazy<[[u64; 64]; 6]> = Lazy::new(|| {
    let mut r = rng();
    let mut keys = [[0; 64]; 6];
    let mut dupe_keys = Vec::new();
    (0..6).for_each(|piece_type| {
        for square in 0..64 {
            let key = r.next_u64();
            assert!(!dupe_keys.contains(&key));
            dupe_keys.push(key);
            keys[piece_type][square] = key;
        }
    });
    keys
});

pub type SliderDirections = [(i8, i8); 4];

pub const ROOK_DIRECTIONS: SliderDirections = [(0, 1), (1, 0), (0, -1), (-1, 0)];
pub const BISHOP_DIRECTIONS: SliderDirections = [(1, 1), (1, -1), (-1, 1), (-1, -1)];

pub struct MagicTableEntry {
    pub mask: u64,
    pub magic: u64,
    pub shift: u8,
    pub offset: u32,
}

pub const DEFAULT_ATTACKS: [Bitboard; 64] = [
    Bitboard(0x102),
    Bitboard(0x50800),
    Bitboard(0xa00),
    Bitboard(0x1c14),
    Bitboard(0x3828),
    Bitboard(0x5000),
    Bitboard(0xa01000),
    Bitboard(0x8040),
    Bitboard(0x20000),
    Bitboard(0x50000),
    Bitboard(0xa0000),
    Bitboard(0x140000),
    Bitboard(0x280000),
    Bitboard(0x500000),
    Bitboard(0xa00000),
    Bitboard(0x400000),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0x20000000000),
    Bitboard(0x50000000000),
    Bitboard(0xa0000000000),
    Bitboard(0x140000000000),
    Bitboard(0x280000000000),
    Bitboard(0x500000000000),
    Bitboard(0xa00000000000),
    Bitboard(0x400000000000),
    Bitboard(0x201000000000000),
    Bitboard(0x8050000000000),
    Bitboard(0xa000000000000),
    Bitboard(0x141c000000000000),
    Bitboard(0x2838000000000000),
    Bitboard(0x50000000000000),
    Bitboard(0x10a00000000000),
    Bitboard(0x4080000000000000),
];

pub const DEFAULT_ATTACKED_BY: [Bitboard; 64] = [
    Bitboard(0),
    Bitboard(0x1),
    Bitboard(0x8),
    Bitboard(0x10),
    Bitboard(0x8),
    Bitboard(0x10),
    Bitboard(0x80),
    Bitboard(0),
    Bitboard(0x1),
    Bitboard(0x4),
    Bitboard(0x8),
    Bitboard(0x1e),
    Bitboard(0x78),
    Bitboard(0x10),
    Bitboard(0x20),
    Bitboard(0x80),
    Bitboard(0x202),
    Bitboard(0x500),
    Bitboard(0xa02),
    Bitboard(0x1400),
    Bitboard(0x2800),
    Bitboard(0x5040),
    Bitboard(0xa000),
    Bitboard(0x4040),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0),
    Bitboard(0x202000000000000),
    Bitboard(0x5000000000000),
    Bitboard(0x20a000000000000),
    Bitboard(0x14000000000000),
    Bitboard(0x28000000000000),
    Bitboard(0x4050000000000000),
    Bitboard(0xa0000000000000),
    Bitboard(0x4040000000000000),
    Bitboard(0x100000000000000),
    Bitboard(0x400000000000000),
    Bitboard(0x800000000000000),
    Bitboard(0x1e00000000000000),
    Bitboard(0x7800000000000000),
    Bitboard(0x1000000000000000),
    Bitboard(0x2000000000000000),
    Bitboard(0x8000000000000000),
    Bitboard(0),
    Bitboard(0x100000000000000),
    Bitboard(0x800000000000000),
    Bitboard(0x1000000000000000),
    Bitboard(0x800000000000000),
    Bitboard(0x1000000000000000),
    Bitboard(0x8000000000000000),
    Bitboard(0),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameResult {
    Checkmate(Color), // Winner
    Stalemate,
    FiftyMoveRule,
}

pub mod util {
    #[allow(unused_macros)]
    macro_rules! mk_mv {
        ($state:expr, $s:ident, $e:ident, W, $piece:tt) => {
            mk_mv!(@color $state, $s, $e, WHITE, $piece)
        };
        ($state:expr, $s:ident, $e:ident, B, $piece:tt) => {
            mk_mv!(@color $state, $s, $e, BLACK, $piece)
        };

        (@color $state:expr, $s:ident, $e:ident, $c:ident, P) => {
            mk_mv!(@final $state, $s, $e, $c, PAWN)
        };
        (@color $state:expr, $s:ident, $e:ident, $c:ident, N) => {
            mk_mv!(@final $state, $s, $e, $c, KNIGHT)
        };
        (@color $state:expr, $s:ident, $e:ident, $c:ident, B) => {
            mk_mv!(@final $state, $s, $e, $c, BISHOP)
        };
        (@color $state:expr, $s:ident, $e:ident, $c:ident, R) => {
            mk_mv!(@final $state, $s, $e, $c, ROOK)
        };
        (@color $state:expr, $s:ident, $e:ident, $c:ident, Q) => {
            mk_mv!(@final $state, $s, $e, $c, QUEEN)
        };
        (@color $state:expr, $s:ident, $e:ident, $c:ident, K) => {
            mk_mv!(@final $state, $s, $e, $c, KING)
        };

        (@final $state:expr, $s:ident, $e:ident, $c:ident, $p:ident) => {
            $state.make_move(
                &Move {
                    start: Squares::$s,
                    end: Squares::$e,
                },
                &Color::$c,
                &Piece::$p,
            )
        };
    }

    #[allow(unused_macros)]
    macro_rules! mv {
        ($s:ident, $e:ident) => {
            Move {
                start: Squares::$s,
                end: Squares::$e,
            }
        };
    }

    pub fn square_to_algebraic(square: u8) -> String {
        let file = (b'a' + (square % 8)) as char;
        let rank = (b'1' + (square / 8)) as char;
        format!("{}{}", file, rank)
    }

    #[allow(unused_imports)]
    pub(crate) use {mk_mv, mv};
}
