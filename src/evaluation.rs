use crate::{Color, Move, Piece, state::GameState};

const PIECE_VALUES: [i32; 6] = [100, 305, 333, 563, 950, 20000];
const POSITIONAL_SCORES: [[i32; 64]; 6] = [
    // Pawns
    [
        0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 10, -20, -20, 10, 10, 5, 5, -5, -10, 0, 0, -10, -5, 5, 0, 0,
        0, 20, 20, 0, 0, 0, 5, 5, 10, 25, 25, 10, 5, 5, 10, 10, 20, 30, 30, 20, 10, 10, 50, 50, 50,
        50, 50, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0,
    ],
    // Knights
    [
        -50, -40, -30, -30, -30, -30, -40, -50, -40, -20, 0, 5, 5, 0, -20, -40, -30, 5, 10, 15, 15,
        10, 5, -30, -30, 0, 15, 20, 20, 15, 0, -30, -30, 5, 15, 20, 20, 15, 5, -30, -30, 0, 10, 15,
        15, 10, 0, -30, -40, -20, 0, 0, 0, 0, -20, -40, -50, -40, -30, -30, -30, -30, -40, -50,
    ],
    // Bishops
    [
        -20, -10, -10, -10, -10, -10, -10, -20, -10, 5, 0, 0, 0, 0, 5, -10, -10, 10, 10, 10, 10,
        10, 10, -10, -10, 0, 10, 10, 10, 10, 0, -10, -10, 5, 5, 10, 10, 5, 5, -10, -10, 0, 5, 10,
        10, 5, 0, -10, -10, 0, 0, 0, 0, 0, 0, -10, -20, -10, -10, -10, -10, -10, -10, -20,
    ],
    // Rooks
    [
        0, 0, 0, 5, 5, 0, 0, 0, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0,
        0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, 5, 10, 10, 10, 10, 10, 10, 5,
        0, 0, 0, 0, 0, 0, 0, 0,
    ],
    // Queens
    [
        -20, -10, -10, -5, -5, -10, -10, -20, -10, 0, 5, 0, 0, 0, 0, -10, -10, 5, 5, 5, 5, 5, 0,
        -10, 0, 0, 5, 5, 5, 5, 0, -10, -5, 0, 5, 5, 5, 5, 0, -10, -10, 0, 5, 5, 5, 5, 0, -10, -10,
        0, 0, 0, 0, 0, 0, -10, -20, -10, -10, -5, -5, -10, -10, -20,
    ],
    // Kings
    [
        20, 30, 10, 0, 0, 10, 30, 20, 20, 20, 0, 0, 0, 0, 20, 20, -10, -20, -20, -20, -20, -20,
        -20, -10, -20, -30, -30, -40, -40, -30, -30, -20, -30, -40, -40, -50, -50, -40, -40, -30,
        -30, -40, -40, -50, -50, -40, -40, -30, -30, -40, -40, -50, -50, -40, -40, -30, -30, -40,
        -40, -50, -50, -40, -40, -30,
    ],
];

fn material_count(state: &GameState, c: Color) -> f32 {
    let pieces = state.pieces_for_color(c);
    let pawns = state.piece_bitboard(Piece::PAWN) & pieces;
    let knights = state.piece_bitboard(Piece::KNIGHT) & pieces;
    let bishops = state.piece_bitboard(Piece::BISHOP) & pieces;
    let rooks = state.piece_bitboard(Piece::ROOK) & pieces;
    let queens = state.piece_bitboard(Piece::QUEEN) & pieces;

    let p = pawns.count_ones() as f32;
    let n = knights.count_ones() as f32 * 305.0;
    let b = bishops.count_ones() as f32 * 333.0;
    let r = rooks.count_ones() as f32 * 563.0;
    let q = queens.count_ones() as f32 * 95.0;

    p + n + b + r + q
}

fn positional_score(state: &GameState, c: Color) -> f32 {
    let mut score = 0;
    let pieces = state.pieces_for_color(c);

    (0..6).for_each(|p: u8| {
        for s in state.piece_bitboard(Piece::from_usize(p)) & pieces {
            if c == Color::BLACK {
                let rank = p / 8;
                let file = p % 8;
                let s = (7 - rank) * 8 + file;
                score += POSITIONAL_SCORES[p as usize][s as usize];
            } else {
                score += POSITIONAL_SCORES[p as usize][s as usize];
            }
        }
    });
    score as f32
}

pub fn evaluate(state: &GameState) -> f32 {
    let to_move = state.to_move();
    let opp = !to_move;

    let our_material = material_count(state, to_move);
    let our_position = positional_score(state, to_move);
    let opp_material = material_count(state, opp);
    let opp_position = positional_score(state, opp);

    our_material + our_position - opp_material - opp_position
}

/// Order moves by their score (best first)
pub fn order_moves(state: &GameState, moves: &mut [Move], color: Color) {
    moves.sort_by_cached_key(|&m| -score_move(state, m, color));
}

pub fn score_move(state: &GameState, m: Move, color: Color) -> i32 {
    let mut score = 0;

    // Check if this is a capture (MVV-LVA: Most Valuable Victim - Least Valuable Attacker)
    if let Some(captured_piece) = state.piece_at(m.end) {
        let attacker_piece = state.piece_at(m.start).unwrap();
        // Value of captured piece (victim) minus value of attacker
        score += PIECE_VALUES[captured_piece as usize] * 10 - PIECE_VALUES[attacker_piece as usize];
    }

    // Check if this move gives check (simulate move and see if opponent king is in check)
    let mut temp_state = *state;
    let piece = state.piece_at(m.start).unwrap();
    temp_state.make_move(m, color, piece);

    if let Some(checked_color) = temp_state.in_check()
        && checked_color != color
    {
        score += 10000;
    }

    // Bonus for pawn promotions (they auto-promote to queen)
    let end_rank = m.end / 8;
    if let Some(Piece::PAWN) = state.piece_at(m.start)
        && ((end_rank == 7 && color == Color::WHITE) || (end_rank == 0 && color == Color::BLACK))
    {
        score += 12000;
    }

    score
}
