use crate::{Color, Piece, state::GameState};

pub fn material_count(state: &GameState, c: &Color) -> f32 {
    let pieces = state.pieces_for_color(c);
    let pawns = state.piece_bitboard(&Piece::PAWN) & pieces;
    let knights = state.piece_bitboard(&Piece::KNIGHT) & pieces;
    let bishops = state.piece_bitboard(&Piece::BISHOP) & pieces;
    let rooks = state.piece_bitboard(&Piece::ROOK) & pieces;
    let queens = state.piece_bitboard(&Piece::QUEEN) & pieces;

    let p = pawns.count_ones() as f32;
    let n = knights.count_ones() as f32 * 3.05;
    let b = bishops.count_ones() as f32 * 3.33;
    let r = rooks.count_ones() as f32 * 5.63;
    let q = queens.count_ones() as f32 * 9.5;

    p + n + b + r + q
}
