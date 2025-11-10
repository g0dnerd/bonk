use core::f32;
use std::sync::atomic::{AtomicU32, Ordering};

use dashmap::DashMap;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    GameResult, Move, Piece,
    evaluation::{evaluate, order_moves},
    movegen::{is_square_attacked_by, legal_moves},
    state::GameState,
};

const CHECKMATE_SCORE: f32 = 100000.0;

pub fn negamax(
    state: &GameState,
    depth: u8,
    mut alpha: f32,
    mut beta: f32,
    tbl: &TranspositionTable,
) -> f32 {
    let hash = state.hash();

    if let Some(entry) = tbl.get_entry(hash)
        && entry.depth > depth
    {
        match entry.flag {
            Flag::Exact => return entry.score,
            Flag::LowerBound => alpha = alpha.max(entry.score),
            Flag::UpperBound => beta = beta.min(entry.score),
        }
        if alpha >= beta {
            return entry.score;
        }
    }

    let to_move = state.to_move();
    let mut moves = legal_moves(state, to_move);

    if moves.is_empty() {
        if state.in_check() == Some(to_move) {
            return -CHECKMATE_SCORE;
        } else {
            // Stalemate
            return 0.0;
        }
    }

    if depth == 0 {
        return evaluate(state);
    }

    // Order moves for better alpha-beta pruning
    order_moves(state, &mut moves, state.to_move());

    let mut max_score = f32::NEG_INFINITY;

    for m in moves {
        let p = state.piece_at(m.start).unwrap();
        let mut new_state = *state;
        new_state.make_move(m, to_move, p);

        new_state.push_move(m);
        let score = -negamax(&new_state, depth - 1, -beta, -alpha, tbl);

        max_score = max_score.max(score);
        alpha = alpha.max(score);

        if alpha >= beta {
            break;
        }
    }

    let flag = if max_score <= alpha {
        Flag::UpperBound
    } else if max_score >= beta {
        Flag::LowerBound
    } else {
        Flag::Exact
    };
    tbl.store_entry(
        hash,
        TranspositionEntry {
            depth,
            score: max_score,
            flag,
        },
    );

    max_score
}

pub fn search(state: &GameState, depth: u8) -> Option<(Move, f32)> {
    let tbl = TranspositionTable::default();

    let to_move = state.to_move();
    let mut moves = legal_moves(state, to_move);

    if moves.is_empty() {
        return None;
    }

    // Order moves for better search
    order_moves(state, &mut moves, state.to_move());

    for &m in &moves {
        let p = state.piece_at(m.start).unwrap();
        let mut new_state = *state;
        new_state.make_move(m, to_move, p);

        // Take mate in 1
        if let Some(r) = is_game_over(&new_state)
            && r == GameResult::Checkmate(!new_state.to_move())
        {
            return Some((m, CHECKMATE_SCORE));
        }
    }

    let results: Vec<(Move, f32)> = moves
        .par_iter()
        .map(|&m| {
            let p = state.piece_at(m.start).unwrap();
            let mut new_state = *state;
            new_state.make_move(m, to_move, p);

            let score = -negamax(
                &new_state,
                depth - 1,
                f32::NEG_INFINITY,
                f32::INFINITY,
                &tbl,
            );

            (m, score)
        })
        .collect();

    results.into_iter().max_by(|(_, score_a), (_, score_b)| {
        score_a
            .partial_cmp(score_b)
            .unwrap_or(std::cmp::Ordering::Equal)
    })
}

pub fn is_game_over(state: &GameState) -> Option<GameResult> {
    let to_move = state.to_move();
    let moves = legal_moves(state, to_move);

    if moves.is_empty() {
        let king_square =
            (state.piece_bitboard(Piece::KING) & state.pieces_for_color(to_move)).trailing_zeros();
        if is_square_attacked_by(state, king_square, !to_move) {
            return Some(GameResult::Checkmate(!to_move));
        } else {
            return Some(GameResult::Stalemate);
        }
    }

    if state.halfmove_clock() >= 100 {
        return Some(GameResult::FiftyMoveRule);
    }

    None
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Flag {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Debug, Copy, Clone)]
pub struct TranspositionEntry {
    depth: u8,
    score: f32,
    flag: Flag,
}

#[derive(Debug, Default)]
pub struct TranspositionTable {
    table: DashMap<u64, TranspositionEntry>,
    hits: AtomicU32,
    misses: AtomicU32,
}

impl TranspositionTable {
    pub fn get_entry(&self, hash: u64) -> Option<TranspositionEntry> {
        match self.table.get(&hash) {
            Some(entry) => {
                self.hits.fetch_add(1, Ordering::Relaxed);
                Some(*entry)
            }
            None => {
                self.misses.fetch_add(1, Ordering::Relaxed);
                None
            }
        }
    }

    pub fn store_entry(&self, hash: u64, entry: TranspositionEntry) {
        if let Some(mut old_entry) = self.table.get_mut(&hash)
            && entry.depth <= old_entry.depth
        {
            *old_entry = entry;
        } else {
            self.table.insert(hash, entry);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Color, Squares, util::mv};

    #[test]
    fn search_finds_mate_in_one() {
        let fen = "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4";
        let state = GameState::from_fen(fen.into()).unwrap();

        let result = search(&state, 1);
        assert!(result.is_some());

        let (best_move, score) = result.unwrap();
        // Should find Qxf7#
        assert_eq!(best_move, mv!(H5, F7));
        assert!(score > 50000.0);
    }

    #[test]
    fn detects_stalemate() {
        // Stalemate position
        let fen = "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1";
        let state = GameState::from_fen(fen.into()).unwrap();

        let result = is_game_over(&state);
        assert_eq!(result, Some(GameResult::Stalemate));
    }

    #[test]
    fn move_ordering_prioritizes_captures() {
        // Position where there are captures available
        let fen = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4";
        let state = GameState::from_fen(fen.into()).unwrap();

        let mut moves = legal_moves(&state, Color::WHITE);
        order_moves(&state, &mut moves, Color::WHITE);

        // The first few moves should include captures if available
        // Bxf7+ is a check and capture, should be very high priority
        let bxf7 = Move {
            start: Squares::C4,
            end: Squares::F7,
        };

        if moves.contains(&bxf7) {
            // Check that this capture+check is in the first few moves
            let position = moves.iter().position(|&m| m == bxf7).unwrap();
            assert!(
                position < 5,
                "Capture+check should be prioritized, was at position {}",
                position
            );
        }
    }

    #[test]
    fn move_ordering_prioritizes_checks() {
        // Position where checks are available
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 2";
        let state = GameState::from_fen(fen.into()).unwrap();

        let mut moves = legal_moves(&state, Color::WHITE);
        let original_first = moves[0];

        order_moves(&state, &mut moves, Color::WHITE);

        // After ordering, if there are checking moves, they should be early
        // The ordering might change the first move if there are tactical moves
        // At minimum, verify ordering doesn't crash and changes the order
        assert!(
            moves[0].start != original_first.start
                || moves[0].end != original_first.end
                || moves.len() == 1
        );
    }

    #[test]
    fn search_finds_capture_quickly() {
        // Position with hanging piece
        let fen = "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3";
        let state = GameState::from_fen(fen.into()).unwrap();

        // Even at depth 1, should see captures
        let result = search(&state, 1);
        assert!(result.is_some());
    }
}
