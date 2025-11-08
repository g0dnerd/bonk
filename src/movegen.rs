use crate::{
    BISHOP_DIRECTIONS, Castling, Color, MagicTableEntry, Move, Piece, ROOK_DIRECTIONS, Square,
    Squares, bitboard::Bitboard, movegen::moves::*, state::GameState, try_square_offset,
};

pub mod magics;
pub mod moves;

pub const PAWN_ATTACKS: [[Bitboard; 8]; 2] = [
    [
        Bitboard(0x20000),
        Bitboard(0x50000),
        Bitboard(0xA0000),
        Bitboard(0x140000),
        Bitboard(0x280000),
        Bitboard(0x500000),
        Bitboard(0xA00000),
        Bitboard(0x400000),
    ],
    [
        Bitboard(0x2),
        Bitboard(0x5),
        Bitboard(0xA),
        Bitboard(0x14),
        Bitboard(0x28),
        Bitboard(0x50),
        Bitboard(0xa0),
        Bitboard(0x40),
    ],
];

pub const KNIGHT_MOVES: [Bitboard; 64] = [
    Bitboard(0x20400),
    Bitboard(0x50800),
    Bitboard(0xa1100),
    Bitboard(0x142200),
    Bitboard(0x284400),
    Bitboard(0x508800),
    Bitboard(0xa01000),
    Bitboard(0x402000),
    Bitboard(0x2040004),
    Bitboard(0x5080008),
    Bitboard(0xa110011),
    Bitboard(0x14220022),
    Bitboard(0x28440044),
    Bitboard(0x50880088),
    Bitboard(0xa0100010),
    Bitboard(0x40200020),
    Bitboard(0x204000402),
    Bitboard(0x508000805),
    Bitboard(0xa1100110a),
    Bitboard(0x1422002214),
    Bitboard(0x2844004428),
    Bitboard(0x5088008850),
    Bitboard(0xa0100010a0),
    Bitboard(0x4020002040),
    Bitboard(0x20400040200),
    Bitboard(0x50800080500),
    Bitboard(0xa1100110a00),
    Bitboard(0x142200221400),
    Bitboard(0x284400442800),
    Bitboard(0x508800885000),
    Bitboard(0xa0100010a000),
    Bitboard(0x402000204000),
    Bitboard(0x2040004020000),
    Bitboard(0x5080008050000),
    Bitboard(0xa1100110a0000),
    Bitboard(0x14220022140000),
    Bitboard(0x28440044280000),
    Bitboard(0x50880088500000),
    Bitboard(0xa0100010a00000),
    Bitboard(0x40200020400000),
    Bitboard(0x204000402000000),
    Bitboard(0x508000805000000),
    Bitboard(0xa1100110a000000),
    Bitboard(0x1422002214000000),
    Bitboard(0x2844004428000000),
    Bitboard(0x5088008850000000),
    Bitboard(0xa0100010a0000000),
    Bitboard(0x4020002040000000),
    Bitboard(0x400040200000000),
    Bitboard(0x800080500000000),
    Bitboard(0x1100110a00000000),
    Bitboard(0x2200221400000000),
    Bitboard(0x4400442800000000),
    Bitboard(0x8800885000000000),
    Bitboard(0x100010a000000000),
    Bitboard(0x2000204000000000),
    Bitboard(0x4020000000000),
    Bitboard(0x8050000000000),
    Bitboard(0x110a0000000000),
    Bitboard(0x22140000000000),
    Bitboard(0x44280000000000),
    Bitboard(0x88500000000000),
    Bitboard(0x10a00000000000),
    Bitboard(0x20400000000000),
];

pub fn magic_tbl_idx(entry: &MagicTableEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let idx = (hash >> entry.shift) as usize;
    entry.offset as usize + idx
}

pub fn knight_moves(s: Square) -> Bitboard {
    KNIGHT_MOVES[s as usize]
}

pub fn pawn_attacks(s: Square, c: &Color) -> Bitboard {
    let file = (s % 8) as usize;
    let rank = ((s / 8) as usize - 1).clamp(0, 5);
    PAWN_ATTACKS[*c as usize][file] << (8 * rank)
}

// Possible pawn moves that do not check positional legality (e.g. whether or not your king would
// be left in check after making a move).
pub fn pawn_moves(state: &GameState, s: Square, c: &Color) -> Bitboard {
    let mut moves = Bitboard(0);

    let direction = match c {
        Color::WHITE => 1,
        Color::BLACK => -1,
    };

    // Check if the square one ahead is within bounds
    if let Some(offs) = try_square_offset(s, 0, direction)
        && state.is_square_empty(offs)
    {
        moves |= offs;
        let rank = s / 8;
        if (rank == 1 && c == &Color::WHITE) || (rank == 6 && c == &Color::BLACK) {
            let two_ahead = (s as i8 + 16 * direction) as u8;
            if state.is_square_empty(two_ahead) {
                moves |= two_ahead;
            }
        }
    }

    // Check for captures
    if let Some(offs) = try_square_offset(s, -1, direction)
        && (!state.is_square_empty(offs) || state.en_passant() == Some(offs))
    {
        moves |= offs;
    }
    if let Some(offs) = try_square_offset(s, 1, direction)
        && (!state.is_square_empty(offs) || state.en_passant() == Some(offs))
    {
        moves |= offs;
    }

    moves
}

pub fn king_moves(state: &GameState, s: Square, c: &Color) -> Bitboard {
    let mut moves = Bitboard(0);
    for (dx, dy) in ROOK_DIRECTIONS {
        if let Some(offs) = try_square_offset(s, dx, dy) {
            moves |= offs;
        }
    }
    for (dx, dy) in BISHOP_DIRECTIONS {
        if let Some(offs) = try_square_offset(s, dx, dy) {
            moves |= offs;
        }
    }

    if state.in_check().is_none() {
        let castling_rights = state.castling_rights();
        match c {
            Color::WHITE => {
                if castling_rights & Castling::WHITE_KINGSIDE != 0
                    && state.is_square_empty(Squares::F1)
                    && state.is_square_empty(Squares::G1)
                {
                    moves |= Squares::G1;
                }
                if castling_rights & Castling::WHITE_QUEENSIDE != 0
                    && state.is_square_empty(Squares::B1)
                    && state.is_square_empty(Squares::C1)
                    && state.is_square_empty(Squares::D1)
                {
                    moves |= Squares::C1;
                }
            }
            Color::BLACK => {
                if castling_rights & Castling::BLACK_KINGSIDE != 0
                    && state.is_square_empty(Squares::F8)
                    && state.is_square_empty(Squares::G8)
                {
                    moves |= Squares::G8;
                }
                if castling_rights & Castling::BLACK_QUEENSIDE != 0
                    && state.is_square_empty(Squares::C8)
                    && state.is_square_empty(Squares::D8)
                {
                    moves |= Squares::C8;
                }
            }
        }
    }

    moves
}

pub fn blockers_from_position(state: &GameState, s: Square, p: &Piece) -> Bitboard {
    let blockers = match p {
        Piece::ROOK => Bitboard(ROOK_MAGICS[s as usize].mask),
        Piece::BISHOP => Bitboard(BISHOP_MAGICS[s as usize].mask),
        Piece::QUEEN => Bitboard(ROOK_MAGICS[s as usize].mask | BISHOP_MAGICS[s as usize].mask),
        _ => unreachable!(),
    };
    blockers & state.all_pieces()
}

// Slider moves without positional legality computed from magics
pub fn slider_moves(state: &GameState, s: Square, p: &Piece) -> Bitboard {
    let blockers = blockers_from_position(state, s, p);

    match p {
        Piece::ROOK => Bitboard(ROOK_MOVES[magic_tbl_idx(&ROOK_MAGICS[s as usize], blockers)]),
        Piece::BISHOP => {
            Bitboard(BISHOP_MOVES[magic_tbl_idx(&BISHOP_MAGICS[s as usize], blockers)])
        }
        Piece::QUEEN => {
            Bitboard(ROOK_MOVES[magic_tbl_idx(&ROOK_MAGICS[s as usize], blockers)])
                | Bitboard(BISHOP_MOVES[magic_tbl_idx(&BISHOP_MAGICS[s as usize], blockers)])
        }
        _ => unreachable!(),
    }
}

pub fn pseudolegal_for_piece(state: &GameState, s: Square, c: &Color, p: &Piece) -> Bitboard {
    match p {
        // Keep only pawn attacks that point to an opposing piece
        Piece::PAWN => (pawn_attacks(s, c) & state.pieces_for_color(&!c)) | pawn_moves(state, s, c),
        Piece::KNIGHT => knight_moves(s),
        Piece::BISHOP | Piece::ROOK | Piece::QUEEN => slider_moves(state, s, p),
        Piece::KING => king_moves(state, s, c),
    }
}

pub fn moves_for_piece(state: &GameState, s: Square, c: &Color, p: &Piece) -> Bitboard {
    let moves = pseudolegal_for_piece(state, s, c, p);
    moves & !state.pieces_for_color(c)
}

// 1. Iterate over all pieces for current color
//  a. Get all pseudo-legal moves for that piece
// 2. Remove all moves that would leave the king in check
// 3. ???
// 4. Profit
pub fn legal_moves(state: &GameState, c: &Color) -> Vec<Move> {
    let mut moves: Vec<Move> = Vec::new();
    let pieces = state.pieces_for_color(c);

    for s in pieces {
        let p = state.piece_at(s).unwrap();
        let piece_moves = moves_for_piece(state, s, c, &p);
        for end in piece_moves {
            let mut tmp_state = *state;
            let candidate_move = Move { start: s, end };
            tmp_state.make_move(&candidate_move, c, &p);

            match &tmp_state.in_check() {
                Some(checked) => {
                    if checked != c {
                        moves.push(candidate_move)
                    }
                }
                None => {
                    moves.push(candidate_move);
                }
            }
        }
    }

    moves
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        state::GameState,
        util::{mk_mv, mv},
    };

    #[test]
    fn pawn_moves_from_default() {
        let game = GameState::default();
        {
            let moves = pawn_moves(&game, Squares::E2, &Color::WHITE);
            assert_eq!(moves, Bitboard(0x10100000));
        }
        {
            let moves = pawn_moves(&game, Squares::E7, &Color::BLACK);
            assert_eq!(moves, Bitboard(0x101000000000));
        }
    }

    #[test]
    fn king_moves_from_default() {
        let game = GameState::default();
        {
            let moves = moves_for_piece(&game, Squares::E1, &Color::WHITE, &Piece::KING);
            assert_eq!(moves, Bitboard(0));
        }
        {
            let moves = moves_for_piece(&game, Squares::E8, &Color::BLACK, &Piece::KING);
            assert_eq!(moves, Bitboard(0));
        }
    }

    #[test]
    fn slider_moves_from_default() {
        let game = GameState::default();
        let mut moves = Vec::new();
        moves.push(moves_for_piece(
            &game,
            Squares::C1,
            &Color::WHITE,
            &Piece::BISHOP,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::C8,
            &Color::BLACK,
            &Piece::BISHOP,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::H1,
            &Color::WHITE,
            &Piece::ROOK,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::H8,
            &Color::BLACK,
            &Piece::ROOK,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::D1,
            &Color::WHITE,
            &Piece::QUEEN,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::D8,
            &Color::BLACK,
            &Piece::QUEEN,
        ));

        for m in moves {
            assert_eq!(m, Bitboard(0));
        }
    }

    #[test]
    fn slider_moves_from_fen() {
        // 1. e4 b6 2. h4 a5
        let fen = "rnbqkbnr/2pppppp/1p6/p7/4P2P/8/PPPP1PP1/RNBQKBNR w KQkq - 0 3";
        let game = GameState::from_fen(fen.into()).unwrap();

        let moves_white_queen = moves_for_piece(&game, Squares::D1, &Color::WHITE, &Piece::QUEEN);
        assert_eq!(moves_white_queen, Bitboard(0x8040201000));

        let moves_white_bishop_c1 =
            moves_for_piece(&game, Squares::C1, &Color::WHITE, &Piece::BISHOP);
        assert_eq!(moves_white_bishop_c1, Bitboard(0));

        let moves_white_bishop_f1 =
            moves_for_piece(&game, Squares::F1, &Color::WHITE, &Piece::BISHOP);
        assert_eq!(moves_white_bishop_f1, Bitboard(0x10204081000));

        let moves_white_rook_a1 = moves_for_piece(&game, Squares::A1, &Color::WHITE, &Piece::ROOK);
        assert_eq!(moves_white_rook_a1, Bitboard(0));

        let moves_white_rook_h1 = moves_for_piece(&game, Squares::H1, &Color::WHITE, &Piece::ROOK);
        assert_eq!(moves_white_rook_h1, Bitboard(0x808000));

        let moves_black_queen = moves_for_piece(&game, Squares::D8, &Color::BLACK, &Piece::QUEEN);
        assert_eq!(moves_black_queen, Bitboard(0));

        let moves_black_bishop_c8 =
            moves_for_piece(&game, Squares::C8, &Color::BLACK, &Piece::BISHOP);
        assert_eq!(moves_black_bishop_c8, Bitboard(0x2010000000000));

        let moves_black_bishop_f8 =
            moves_for_piece(&game, Squares::F8, &Color::BLACK, &Piece::BISHOP);
        assert_eq!(moves_black_bishop_f8, Bitboard(0));

        let moves_black_rook_a8 = moves_for_piece(&game, Squares::A8, &Color::BLACK, &Piece::ROOK);
        assert_eq!(moves_black_rook_a8, Bitboard(0x1010000000000));

        let moves_black_rook_h8 = moves_for_piece(&game, Squares::H8, &Color::BLACK, &Piece::ROOK);
        assert_eq!(moves_black_rook_h8, Bitboard(0));
    }

    #[test]
    fn knight_moves_from_default() {
        let game = GameState::default();
        let mut moves = Vec::new();
        let expected: Vec<u64> = vec![0x50000, 0xa00000, 0x50000000000, 0xa00000000000];

        moves.push(moves_for_piece(
            &game,
            Squares::B1,
            &Color::WHITE,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::G1,
            &Color::WHITE,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::B8,
            &Color::BLACK,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::G8,
            &Color::BLACK,
            &Piece::KNIGHT,
        ));

        for (m, e) in moves.into_iter().zip(expected) {
            assert_eq!(m, Bitboard(e));
        }
    }

    #[test]
    fn knight_moves_from_fen() {
        // 1. e4 b6 2. h4 a5 3. Nf3 Nh6 4. d3 Nc6 5. Nbd2 Nd4
        let fen = "r1bqkb1r/2pppppp/1p5n/p7/3nP2P/3P1N2/PPPN1PP1/R1BQKB1R w KQkq - 3 6";
        let game = GameState::from_fen(fen.into()).unwrap();
        let mut moves = Vec::new();
        let expected: Vec<u64> = vec![0x4020002, 0x5008008040, 0x142200221400, 0x4000002040000000];

        moves.push(moves_for_piece(
            &game,
            Squares::D2,
            &Color::WHITE,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::F3,
            &Color::WHITE,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::D4,
            &Color::BLACK,
            &Piece::KNIGHT,
        ));
        moves.push(moves_for_piece(
            &game,
            Squares::H6,
            &Color::BLACK,
            &Piece::KNIGHT,
        ));

        for (m, e) in moves.into_iter().zip(expected) {
            assert_eq!(m, Bitboard(e));
        }
    }

    #[test]
    fn legal_from_default() {
        let game = GameState::default();

        let expected: Vec<Move> = vec![
            mv!(A2, A3),
            mv!(A2, A4),
            mv!(B2, B3),
            mv!(B2, B4),
            mv!(C2, C3),
            mv!(C2, C4),
            mv!(D2, D3),
            mv!(D2, D4),
            mv!(E2, E3),
            mv!(E2, E4),
            mv!(F2, F3),
            mv!(F2, F4),
            mv!(G2, G3),
            mv!(G2, G4),
            mv!(H2, H3),
            mv!(H2, H4),
            mv!(B1, A3),
            mv!(B1, C3),
            mv!(G1, H3),
            mv!(G1, F3),
        ];

        let moves = legal_moves(&game, game.to_move());

        assert_eq!(moves.len(), expected.len());

        for (m, e) in moves.iter().zip(&expected) {
            assert!(expected.contains(m));
            assert!(moves.contains(e));
        }
    }

    #[test]
    fn legal_from_fen() {
        let fen = "r1bqkb1r/2pppppp/1p5n/p7/3nP2P/1P1P1N2/P1PN1PP1/R1BQKB1R b KQkq - 0 6";
        let state = GameState::from_fen(fen.into()).unwrap();

        let expected: Vec<Move> = vec![
            mv!(A5, A4),
            mv!(B6, B5),
            mv!(C7, C5),
            mv!(C7, C6),
            mv!(D7, D5),
            mv!(D7, D6),
            mv!(E7, E5),
            mv!(E7, E6),
            mv!(F7, F5),
            mv!(F7, F6),
            mv!(G7, G5),
            mv!(G7, G6),
            mv!(D4, B3),
            mv!(D4, B5),
            mv!(D4, C2),
            mv!(D4, E2),
            mv!(D4, F3),
            mv!(D4, F5),
            mv!(D4, E6),
            mv!(D4, C6),
            mv!(H6, G4),
            mv!(H6, F5),
            mv!(H6, G8),
            mv!(A8, A7),
            mv!(A8, A6),
            mv!(A8, B8),
            mv!(C8, B7),
            mv!(C8, A6),
            mv!(H8, G8),
        ];

        let moves = legal_moves(&state, state.to_move());

        assert_eq!(moves.len(), expected.len());
        for (m, e) in moves.iter().zip(&expected) {
            assert!(expected.contains(m));
            assert!(moves.contains(e));
        }
    }

    #[test]
    fn legal_moves_no_discovered_check() {
        let fen = "rnbqk1nr/pppp1ppp/8/4p3/1b2P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3";
        let state = GameState::from_fen(fen.into()).unwrap();
        let moves = legal_moves(&state, &Color::WHITE);

        assert!(moves.iter().all(|m| m.start != Squares::D2));
    }

    #[test]
    fn legal_moves_cannot_move_into_check() {
        let fen = "rn1qkbnr/ppp1pppp/8/3p4/3PP1b1/8/PPP2PPP/RNBQKBNR w KQkq - 1 3";
        let state = GameState::from_fen(fen.into()).unwrap();
        let moves = legal_moves(&state, &Color::WHITE);

        assert!(!moves.contains(&mv!(E1, E2)));
    }

    #[test]
    fn legal_with_castling() {
        let fen = "r3k2r/pppq1ppp/2npbn2/2b1p3/2B1P3/2NPBN2/PPPQ1PPP/R3K2R w KQkq - 4 8";
        let state = GameState::from_fen(fen.into()).unwrap();

        let expected: Vec<Move> = vec![
            mv!(A1, B1),
            mv!(A1, C1),
            mv!(A1, D1),
            mv!(E1, D1),
            mv!(E1, F1),
            mv!(E1, E2),
            mv!(E1, C1),
            mv!(E1, G1),
            mv!(H1, G1),
            mv!(H1, F1),
            mv!(A2, A3),
            mv!(A2, A4),
            mv!(B2, B3),
            mv!(B2, B4),
            mv!(D2, D1),
            mv!(D2, C1),
            mv!(D2, E2),
            mv!(G2, G3),
            mv!(G2, G4),
            mv!(H2, H3),
            mv!(H2, H4),
            mv!(C3, B1),
            mv!(C3, D1),
            mv!(C3, E2),
            mv!(C3, D5),
            mv!(C3, B5),
            mv!(C3, A4),
            mv!(D3, D4),
            mv!(E3, D4),
            mv!(E3, F4),
            mv!(E3, G5),
            mv!(E3, H6),
            mv!(E3, C5),
            mv!(F3, G1),
            mv!(F3, H4),
            mv!(F3, G5),
            mv!(F3, E5),
            mv!(F3, D4),
            mv!(C4, D5),
            mv!(C4, E6),
            mv!(C4, B5),
            mv!(C4, A6),
            mv!(C4, B3),
        ];

        let moves = legal_moves(&state, state.to_move());

        // let missing: Vec<&Move> = expected.iter().filter(|m| !moves.contains(m)).collect();
        // let unnecessary: Vec<&Move> = moves.iter().filter(|m| !expected.contains(m)).collect();
        //
        // eprintln!("Missing: {missing:?}");
        // eprintln!("Unnecessary: {unnecessary:?}");

        assert_eq!(moves.len(), expected.len());

        for (m, e) in moves.iter().zip(&expected) {
            assert!(expected.contains(m));
            assert!(moves.contains(e));
        }
    }

    #[test]
    fn legal_with_enpassant() {
        let mut state = GameState::default();
        mk_mv!(state, E2, E4, W, P);
        mk_mv!(state, F7, F5, B, P);
        mk_mv!(state, E4, E5, W, P);
        mk_mv!(state, D7, D5, B, P);

        assert!(state.en_passant().is_some_and(|e| e == Squares::D6));

        let moves = legal_moves(&state, state.to_move());

        assert!(moves.contains(&mv!(E5, D6)));
    }
}
