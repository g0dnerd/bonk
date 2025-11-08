use crate::{
    Castling, CastlingRights, Color, DEFAULT_ATTACKED_BY, DEFAULT_ATTACKS, Move, PIECE_REPR, Piece,
    Square, Squares,
    bitboard::Bitboard,
    movegen::{pawn_attacks, pseudolegal_for_piece},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GameState {
    pieces: [Bitboard; 6],
    colors: [Bitboard; 2],
    to_move: Color,
    castling_rights: CastlingRights,
    en_passant: Option<Square>,
    in_check: Option<Color>,
    halfmove_clock: usize,
    fullmove_clock: usize,
    outgoing_attacks: [Bitboard; 64],
    incoming_attacks: [Bitboard; 64],
}

impl Default for GameState {
    fn default() -> Self {
        let pawns = Bitboard(0xff00000000ff00);
        let knights = Bitboard(0x4200000000000042);
        let bishops = Bitboard(0x2400000000000024);
        let rooks = Bitboard(0x8100000000000081);
        let queens = Bitboard(0x800000000000008);
        let kings = Bitboard(0x1000000000000010);

        let white_pieces = Bitboard(0xffff);
        let black_pieces = Bitboard(0xffff000000000000);

        Self {
            pieces: [pawns, knights, bishops, rooks, queens, kings],
            colors: [white_pieces, black_pieces],
            to_move: Color::WHITE,
            castling_rights: Castling::ALL_LEGAL,
            en_passant: None,
            in_check: None,
            halfmove_clock: 0,
            fullmove_clock: 1,
            outgoing_attacks: DEFAULT_ATTACKS,
            incoming_attacks: DEFAULT_ATTACKED_BY,
        }
    }
}

impl GameState {
    pub fn to_move(&self) -> &Color {
        &self.to_move
    }

    pub fn incoming_attacks(&self, s: Square) -> Bitboard {
        self.incoming_attacks[s as usize]
    }

    pub fn outgoing_attacks_by_square(&self, s: Square) -> Bitboard {
        self.outgoing_attacks[s as usize]
    }

    pub fn piece_bitboards(&self) -> &[Bitboard; 6] {
        &self.pieces
    }

    pub fn pieces_for_color(&self, color: &Color) -> Bitboard {
        self.colors[*color as usize]
    }

    pub fn piece_bitboard(&self, piece: &Piece) -> Bitboard {
        self.pieces[*piece as usize]
    }

    pub fn castling_rights(&self) -> &CastlingRights {
        &self.castling_rights
    }

    pub fn en_passant(&self) -> Option<u8> {
        self.en_passant
    }

    pub fn halfmove_clock(&self) -> usize {
        self.halfmove_clock
    }

    pub fn fullmove_clock(&self) -> usize {
        self.fullmove_clock
    }

    pub fn is_square_empty(&self, s: Square) -> bool {
        !self.all_pieces().contains(s)
    }

    pub fn piece_at(&self, s: Square) -> Option<Piece> {
        (0..=5)
            .find(|&p| self.pieces[p].contains(s))
            .map(Piece::from_usize)
    }

    pub fn color_at(&self, s: Square) -> Option<Color> {
        (0..=1)
            .find(|&p| self.colors[p].contains(s))
            .map(Color::from_usize)
    }

    pub fn all_pieces(&self) -> Bitboard {
        self.colors[0] | self.colors[1]
    }

    pub fn all_sliders(&self) -> Bitboard {
        self.pieces[Piece::ROOK as usize]
            | self.pieces[Piece::BISHOP as usize]
            | self.pieces[Piece::QUEEN as usize]
    }

    pub fn in_check(&self) -> Option<Color> {
        self.in_check
    }

    pub fn from_fen(fen: String) -> Result<Self, String> {
        #[derive(Debug, PartialEq, Eq)]
        enum FenState {
            Placement,
            Color,
            Castling,
            EnPassant,
            Halfmove,
            Fullmove,
        }

        let mut current_square: u8 = 56;

        let mut state = FenState::Placement;
        let mut pawns = Bitboard(0);
        let mut knights = Bitboard(0);
        let mut bishops = Bitboard(0);
        let mut rooks = Bitboard(0);
        let mut queens = Bitboard(0);
        let mut kings = Bitboard(0);

        let mut white_pieces = Bitboard(0);
        let mut black_pieces = Bitboard(0);
        let mut to_move: Option<Color> = None;
        let mut en_passant: Option<Square> = None;
        let mut en_passant_file: Option<u8> = None;
        let mut castling_rights = Castling::NO_LEGAL;
        let mut halfmove_clock: Option<usize> = None;

        let mut halfmove_buffer = Vec::new();
        let mut fullmove_buffer = Vec::new();

        for c in fen.chars() {
            match state {
                FenState::Placement => match c {
                    'p' => {
                        pawns |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'P' => {
                        pawns |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'n' => {
                        knights |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'N' => {
                        knights |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'b' => {
                        bishops |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'B' => {
                        bishops |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'r' => {
                        rooks |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'R' => {
                        rooks |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'q' => {
                        queens |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'Q' => {
                        queens |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'k' => {
                        kings |= current_square;
                        black_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    'K' => {
                        kings |= current_square;
                        white_pieces |= current_square;
                        if current_square % 8 < 7 {
                            current_square += 1;
                        }
                    }
                    '0'..='8' => {
                        let current_rank = current_square / 8;
                        let amount = c.to_digit(10).unwrap() as u8;
                        let mut new_square = current_square + amount;
                        let new_file = new_square % 8;
                        if new_file == 0 {
                            new_square -= 1;
                        }
                        let new_rank = new_square / 8;
                        if new_rank < current_rank {
                            return Err("Too many empty squares in FEN".into());
                        }
                        current_square = new_square;
                        continue;
                    }
                    '/' => {
                        if current_square % 8 == 7 {
                            current_square -= 15;
                            continue;
                        } else {
                            return Err(format!(
                                "Slash (/) before file end in FEN (Square {current_square})"
                            ));
                        }
                    }
                    ' ' => {
                        if current_square == 7 {
                            state = FenState::Color;
                        } else {
                            return Err("Unexpected whitespace in FEN".into());
                        }
                    }
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
                FenState::Color => match c {
                    'w' => to_move = Some(Color::WHITE),
                    'b' => to_move = Some(Color::BLACK),
                    ' ' => {
                        if to_move.is_none() {
                            return Err("Unexpected whitespace in FEN".into());
                        } else {
                            state = FenState::Castling;
                        }
                    }
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
                FenState::Castling => match c {
                    '-' => (),
                    'k' => castling_rights |= Castling::BLACK_KINGSIDE,
                    'K' => castling_rights |= Castling::WHITE_KINGSIDE,
                    'q' => castling_rights |= Castling::BLACK_QUEENSIDE,
                    'Q' => castling_rights |= Castling::WHITE_QUEENSIDE,
                    ' ' => state = FenState::EnPassant,
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
                FenState::EnPassant => match c {
                    '-' => (),
                    'a'..='h' => en_passant_file = Some(97 - c as u8),
                    '0'..='8' => {
                        if let Some(f) = en_passant_file {
                            let rank = c.to_digit(10).unwrap() as u8;
                            let ep: Square = (7 - f) * 8 + rank;
                            if ep > 63 {
                                return Err("Invalid en passant square in FEN".into());
                            }
                            en_passant = Some(ep);
                        } else {
                            return Err("Invalid en passant square in FEN".into());
                        }
                    }
                    ' ' => state = FenState::Halfmove,
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
                FenState::Halfmove => match c {
                    '0'..='9' => {
                        halfmove_buffer.push(c);
                    }
                    ' ' => {
                        if halfmove_buffer.is_empty() {
                            return Err("Missing halfmove clock in FEN".into());
                        }
                        let hm = halfmove_buffer
                            .iter()
                            .collect::<String>()
                            .parse::<usize>()
                            .map_err(|_| "Invalid halfmove clock in FEN".to_string())?;
                        halfmove_clock = Some(hm);
                        state = FenState::Fullmove;
                    }
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
                FenState::Fullmove => match c {
                    '0'..='9' => {
                        fullmove_buffer.push(c);
                    }
                    _ => return Err(format!("Invalid character '{c}' in FEN")),
                },
            }
        }

        if fullmove_buffer.is_empty() {
            return Err("Missing fullmove clock in FEN".into());
        }
        let fullmove_clock = fullmove_buffer
            .iter()
            .collect::<String>()
            .parse::<usize>()
            .map_err(|_| "Invalid fullmove clock in FEN".to_string())?;

        let mut res = Self {
            pieces: [pawns, knights, bishops, rooks, queens, kings],
            colors: [white_pieces, black_pieces],
            to_move: to_move.unwrap(),
            castling_rights,
            en_passant,
            in_check: None,
            halfmove_clock: halfmove_clock.unwrap(),
            fullmove_clock,
            outgoing_attacks: [Bitboard(0); 64],
            incoming_attacks: [Bitboard(0); 64],
        };

        for s in 0..64 {
            if !res.is_square_empty(s) {
                res.update_attacks(s);
            }
        }
        for s in 0..64 {
            res.recalculate_incoming_attacks(s);
        }

        let king_mask = res.piece_bitboard(&Piece::KING) & res.pieces_for_color(res.to_move());
        let king_atx =
            res.incoming_attacks(king_mask.trailing_zeros()) & !res.pieces_for_color(res.to_move());

        if !king_atx.is_empty() {
            res.in_check = Some(*res.to_move());
        }

        Ok(res)
    }

    // 1. Store squares currently attacked by piece to move
    // 2. Store squares currently attacking piece to move
    // 3. Store sliders currently blocked by piece to move:
    //      incoming & sliders (unless start rank or file is 0 or 7)
    // 4. Move piece
    // 5. Update en passant
    // 6. Update castling
    // 7. Update squares attacked by target square
    // 8. Update newly unblocked sliders, if any
    // 8. Update check
    // 9. Update move clocks
    // 10. Update color to move
    pub fn make_move(&mut self, m: &Move, c: &Color, p: &Piece) {
        let start = m.start;
        let end = m.end;
        // eprintln!("Making move {start:?} {end:?}");
        let outgoing_atx_prev = self.outgoing_attacks_by_square(start);
        let incoming_atx_prev = self.incoming_attacks(start);

        let mut unblocked_sliders = if start == Squares::A1
            || start == Squares::H1
            || start == Squares::A8
            || start == Squares::H8
        {
            Bitboard(0)
        } else {
            incoming_atx_prev & self.all_sliders() & !Bitboard::from_square(start)
        };

        self.en_passant = None;
        let mut is_promotion = false;
        // TODO: En passant captures
        let piece_to_capture = self.piece_at(end);
        let mut en_passant_target: Option<Square> = None;

        // Update en passant & castling
        match p {
            Piece::PAWN => {
                self.halfmove_clock = 0;
                let target_rank = end / 8;
                let delta = end.abs_diff(start);
                match c {
                    Color::WHITE => {
                        match delta {
                            16 => self.en_passant = Some(start + 8),
                            15 | 17 => en_passant_target = Some(start + 8),
                            _ => (),
                        }

                        // Auto-promote to queen
                        if target_rank == 7 {
                            is_promotion = true;
                        }
                    }
                    Color::BLACK => {
                        match delta {
                            16 => self.en_passant = Some(start - 8),
                            15 | 17 => en_passant_target = Some(start - 8),
                            _ => (),
                        }
                        // Auto-promote to queen
                        if target_rank == 0 {
                            is_promotion = true;
                        }
                    }
                }
            }
            Piece::KING => {
                match c {
                    Color::WHITE => self.castling_rights &= !Castling::WHITE_CASTLING,
                    Color::BLACK => self.castling_rights &= !Castling::BLACK_CASTLING,
                }
                // Castling move
                if start.abs_diff(end) == 2 {
                    match end {
                        Squares::G1 => {
                            unblocked_sliders |= Squares::F1;
                            unblocked_sliders ^= Squares::H1;
                            self.pieces[Piece::ROOK as usize] ^= Squares::H1;
                            self.pieces[Piece::ROOK as usize] |= Squares::F1;
                            self.colors[*c as usize] |= Squares::F1;
                            self.colors[*c as usize] ^= Squares::H1;
                        }
                        Squares::C1 => {
                            unblocked_sliders |= Squares::D1;
                            unblocked_sliders ^= Squares::A1;
                            self.pieces[Piece::ROOK as usize] ^= Squares::A1;
                            self.pieces[Piece::ROOK as usize] |= Squares::D1;
                            self.colors[*c as usize] |= Squares::D1;
                            self.colors[*c as usize] ^= Squares::A1;
                        }
                        Squares::G8 => {
                            unblocked_sliders |= Squares::F8;
                            unblocked_sliders ^= Squares::H8;
                            self.pieces[Piece::ROOK as usize] ^= Squares::H8;
                            self.pieces[Piece::ROOK as usize] |= Squares::F8;
                            self.colors[*c as usize] |= Squares::F8;
                            self.colors[*c as usize] ^= Squares::H8;
                        }
                        Squares::C8 => {
                            unblocked_sliders |= Squares::D8;
                            unblocked_sliders ^= Squares::A8;
                            self.pieces[Piece::ROOK as usize] ^= Squares::A8;
                            self.pieces[Piece::ROOK as usize] |= Squares::D8;
                            self.colors[*c as usize] |= Squares::D8;
                            self.colors[*c as usize] ^= Squares::A8;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Piece::ROOK => match c {
                Color::WHITE => match start {
                    Squares::A1 => self.castling_rights &= !Castling::WHITE_QUEENSIDE,
                    Squares::H1 => self.castling_rights &= !Castling::WHITE_KINGSIDE,
                    _ => (),
                },
                Color::BLACK => match start {
                    Squares::A8 => self.castling_rights &= !Castling::BLACK_QUEENSIDE,
                    Squares::H8 => self.castling_rights &= !Castling::BLACK_KINGSIDE,
                    _ => (),
                },
            },
            _ => (),
        }

        if let Some(x) = piece_to_capture {
            self.pieces[x as usize] ^= end;
            self.colors[!c as usize] ^= end;
        }

        if let Some(t) = en_passant_target {
            self.pieces[Piece::PAWN as usize] ^= t;
            self.colors[!c as usize] ^= t;
        }

        // Actually move the piece
        self.pieces[*p as usize] ^= start;
        self.colors[*c as usize] ^= start;
        self.pieces[*p as usize] |= end;
        self.colors[*c as usize] |= end;

        if is_promotion {
            self.pieces[Piece::PAWN as usize] ^= end;
            self.pieces[Piece::QUEEN as usize] |= end;
            unblocked_sliders |= end;
        }

        self.outgoing_attacks[usize::from(start)] = Bitboard(0);

        // Remove start square from incoming attack list of all relevant squares
        for s in outgoing_atx_prev {
            self.incoming_attacks[usize::from(s)] ^= start;
        }

        // eprintln!("Updating attacks for unblocked slider {unblocked_sliders:?}\n{self}");
        for s in unblocked_sliders {
            self.update_attacks(s);
        }

        let incoming_atx_new = self.incoming_attacks(end);
        let blocked_sliders = incoming_atx_new & self.all_sliders() & !Bitboard::from_square(end);
        let mut squares_to_recalculate = Bitboard(0);
        for s in blocked_sliders {
            squares_to_recalculate |= self.outgoing_attacks_by_square(s);
        }

        for s in blocked_sliders {
            self.update_attacks(s);
        }
        for s in squares_to_recalculate {
            self.recalculate_incoming_attacks(s);
        }

        self.update_attacks(end);
        self.halfmove_clock += 1;

        if self.to_move == Color::BLACK {
            self.fullmove_clock += 1;
        }

        let king_mask = self.piece_bitboard(&Piece::KING);
        let active_pieces = self.pieces_for_color(c);
        let inactive_pieces = self.pieces_for_color(&!c);
        let active_king_mask = king_mask & active_pieces;
        let inactive_king_mask = king_mask & inactive_pieces;
        let other_king_atx =
            self.incoming_attacks(inactive_king_mask.trailing_zeros()) & active_pieces;
        let own_king_atx =
            self.incoming_attacks(active_king_mask.trailing_zeros()) & inactive_pieces;

        if !other_king_atx.is_empty() {
            self.in_check = Some(!c);
        } else if !own_king_atx.is_empty() {
            self.in_check = Some(*c);
        } else {
            self.in_check = None;
        }

        self.to_move = !self.to_move;
    }

    fn update_attacks(&mut self, s: Square) {
        let p = self.piece_at(s).unwrap();
        let c = self.color_at(s).unwrap();
        let moves = if p != Piece::PAWN {
            pseudolegal_for_piece(self, s, &c, &p)
        } else {
            pawn_attacks(s, &c)
        };

        self.outgoing_attacks[s as usize] = moves;

        for m in moves {
            self.incoming_attacks[m as usize] |= s;
        }
    }

    fn recalculate_incoming_attacks(&mut self, s: Square) {
        let mut attackers = Bitboard(0);
        for origin in 0..64 {
            let attacks = self.outgoing_attacks_by_square(origin);
            if attacks.contains(s) {
                attackers |= origin;
            }
        }
        self.incoming_attacks[s as usize] = attackers;
    }
}

impl std::fmt::Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                let s = rank * 8 + file;
                if let Some(p) = self.piece_at(s) {
                    let c = self.color_at(s).unwrap();
                    write!(f, "{} ", PIECE_REPR[c as usize][p as usize])?;
                } else {
                    write!(f, "- ")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        movegen::legal_moves,
        util::{mk_mv, mv},
    };

    use super::*;

    #[test]
    fn state_from_fen_default() {
        let starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let state = GameState::from_fen(starting_fen.to_string()).unwrap();
        let default_state = GameState::default();

        assert_eq!(state.pieces, default_state.pieces);
        assert_eq!(state.colors, default_state.colors);
        assert_eq!(state.to_move, default_state.to_move);
        assert_eq!(state.castling_rights, default_state.castling_rights);
        assert_eq!(state.en_passant, default_state.en_passant);
        assert_eq!(state.in_check, default_state.in_check);
        assert_eq!(state.halfmove_clock, default_state.halfmove_clock);
        assert_eq!(state.fullmove_clock, default_state.fullmove_clock);

        for s in 0..64 {
            assert_eq!(state.outgoing_attacks[s], default_state.outgoing_attacks[s]);
            assert_eq!(state.incoming_attacks[s], default_state.incoming_attacks[s]);
        }
    }

    #[test]
    fn state_from_fen_e4_c5_nf3() {
        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2";
        let fen_state = GameState::from_fen(fen.to_string()).unwrap();
        let pawns = fen_state.piece_bitboard(&Piece::PAWN);
        let knights = fen_state.piece_bitboard(&Piece::KNIGHT);
        let bishops = fen_state.piece_bitboard(&Piece::BISHOP);
        let rooks = fen_state.piece_bitboard(&Piece::ROOK);
        let queens = fen_state.piece_bitboard(&Piece::QUEEN);
        let kings = fen_state.piece_bitboard(&Piece::KING);

        assert_eq!(pawns, Bitboard(0xfb00041000ef00));
        assert_eq!(knights, Bitboard(0x4200000000200002));
        assert_eq!(bishops, Bitboard(0x2400000000000024));
        assert_eq!(rooks, Bitboard(0x8100000000000081));
        assert_eq!(queens, Bitboard(0x800000000000008));
        assert_eq!(kings, Bitboard(0x1000000000000010));

        let white_pieces = fen_state.pieces_for_color(&Color::WHITE);
        let black_pieces = fen_state.pieces_for_color(&Color::BLACK);

        assert_eq!(white_pieces, Bitboard(0x1020efbf));
        assert_eq!(black_pieces, Bitboard(0xfffb000400000000));

        assert_eq!(fen_state.to_move(), &Color::BLACK);
        assert_eq!(fen_state.castling_rights(), &Castling::ALL_LEGAL);
        assert!(fen_state.en_passant().is_none());
        assert_eq!(fen_state.halfmove_clock(), 1);
        assert_eq!(fen_state.fullmove_clock(), 2);
    }

    #[test]
    fn state_from_fen_complex() {
        let fen = "8/6p1/3R1r2/q1p1p2Q/4k3/4B3/5PPK/8 w - - 15 62";
        let fen_state = GameState::from_fen(fen.to_string()).unwrap();

        let pawns = fen_state.piece_bitboard(&Piece::PAWN);
        let knights = fen_state.piece_bitboard(&Piece::KNIGHT);
        let bishops = fen_state.piece_bitboard(&Piece::BISHOP);
        let rooks = fen_state.piece_bitboard(&Piece::ROOK);
        let queens = fen_state.piece_bitboard(&Piece::QUEEN);
        let kings = fen_state.piece_bitboard(&Piece::KING);

        assert_eq!(pawns, Bitboard(0x40001400006000));
        assert_eq!(knights, Bitboard(0));
        assert_eq!(bishops, Bitboard(0x100000));
        assert_eq!(rooks, Bitboard(43980465111040));
        assert_eq!(queens, Bitboard(0x8100000000));
        assert_eq!(kings, Bitboard(0x10008000));

        let white_pieces = fen_state.pieces_for_color(&Color::WHITE);
        let black_pieces = fen_state.pieces_for_color(&Color::BLACK);

        assert_eq!(white_pieces, Bitboard(0x8800010e000));
        assert_eq!(black_pieces, Bitboard(0x40201510000000));

        assert_eq!(fen_state.to_move(), &Color::WHITE);
        assert_eq!(fen_state.castling_rights(), &Castling::NO_LEGAL);
        assert!(fen_state.en_passant().is_none());
        assert_eq!(fen_state.halfmove_clock(), 15);
        assert_eq!(fen_state.fullmove_clock(), 62);
    }

    #[test]
    fn castling_rights_removal() {
        let mut state = GameState::default();

        mk_mv!(state, E2, E4, W, P);
        mk_mv!(state, E7, E5, B, P);
        mk_mv!(state, G1, F3, W, N);
        mk_mv!(state, D8, E7, B, Q);
        mk_mv!(state, F1, E2, W, B);
        mk_mv!(state, E7, D8, B, Q);
        mk_mv!(state, E1, F1, W, K);

        assert_eq!(
            state.castling_rights(),
            &(Castling::ALL_LEGAL ^ Castling::WHITE_CASTLING)
        );

        state = GameState::default();
        mk_mv!(state, A2, A3, W, P);
        mk_mv!(state, A7, A6, B, P);
        mk_mv!(state, A1, A2, W, R);

        assert_eq!(
            state.castling_rights(),
            &(Castling::ALL_LEGAL ^ Castling::WHITE_QUEENSIDE)
        );
    }

    #[test]
    fn attacks_after_moves() {
        let mut state = GameState::default();

        mk_mv!(state, E2, E4, W, P);
        mk_mv!(state, E7, E5, B, P);
        mk_mv!(state, G1, F3, W, N);
        mk_mv!(state, B8, C6, B, N);
        mk_mv!(state, F1, B5, W, B);

        let mut expected_incoming = DEFAULT_ATTACKED_BY;
        let mut expected_outgoing = DEFAULT_ATTACKS;

        expected_outgoing[3] |= Squares::F3;
        expected_outgoing[5] = Bitboard(0);
        expected_outgoing[6] = Bitboard(0);
        expected_outgoing[7] |= Bitboard(0x30);
        expected_outgoing[12] = Bitboard(0);
        expected_outgoing[21] = Bitboard(0x5088008850);
        expected_outgoing[28] = Bitboard(0x2800000000);
        expected_outgoing[33] = Bitboard(0x50005081020);
        expected_outgoing[36] = Bitboard(0x28000000);
        expected_outgoing[42] = Bitboard(0xa1100110a000000);
        expected_outgoing[52] = Bitboard(0);
        expected_outgoing[56] = Bitboard(0x601000000000000);
        expected_outgoing[57] = Bitboard(0);
        expected_outgoing[59] |= Bitboard(0x141c204080000000);
        expected_outgoing[61] |= Bitboard(0x80402010000);

        expected_incoming[12] &= !Bitboard::from_square(5);
        expected_incoming[12] &= !Bitboard::from_square(6);
        expected_incoming[14] &= !Bitboard::from_square(5);
        expected_incoming[19] &= !Bitboard::from_square(12);
        expected_incoming[21] &= !Bitboard::from_square(12);
        expected_incoming[21] &= !Bitboard::from_square(6);
        expected_incoming[23] &= !Bitboard::from_square(6);
        expected_incoming[40] &= !Bitboard::from_square(57);
        expected_incoming[42] &= !Bitboard::from_square(57);
        expected_incoming[43] &= !Bitboard::from_square(52);
        expected_incoming[45] &= !Bitboard::from_square(52);
        expected_incoming[51] &= !Bitboard::from_square(57);

        expected_incoming[4] |= Squares::H1;
        expected_incoming[4] |= Squares::F3;
        expected_incoming[5] |= Squares::B5;
        expected_incoming[5] |= Squares::H1;
        expected_incoming[6] |= Squares::F3;
        expected_incoming[11] |= Squares::F3;
        expected_incoming[12] |= Squares::B5;
        expected_incoming[15] |= Squares::F3;
        expected_incoming[16] |= Squares::F8;
        expected_incoming[19] |= Squares::B5;
        expected_incoming[21] |= Squares::D1;
        expected_incoming[24] |= Squares::B5;
        expected_incoming[25] |= Squares::C6;
        expected_incoming[25] |= Squares::F8;
        expected_incoming[26] |= Squares::B5;
        expected_incoming[27] |= Squares::C6;
        expected_incoming[27] |= Squares::F3;
        expected_incoming[27] |= Squares::E5;
        expected_incoming[29] |= Squares::E5;
        expected_incoming[31] |= Squares::F3;
        expected_incoming[31] |= Squares::D8;
        expected_incoming[32] |= Squares::C6;
        expected_incoming[34] |= Squares::F8;
        expected_incoming[35] |= Squares::E4;
        expected_incoming[36] |= Squares::F3;
        expected_incoming[36] |= Squares::C6;
        expected_incoming[37] |= Squares::E4;
        expected_incoming[38] |= Squares::F3;
        expected_incoming[38] |= Squares::D8;
        expected_incoming[40] |= Squares::B5;
        expected_incoming[42] |= Squares::B5;
        expected_incoming[43] |= Squares::F8;
        expected_incoming[45] |= Squares::D8;
        expected_incoming[48] |= Squares::C6;
        expected_incoming[52] |= Squares::C6;
        expected_incoming[57] |= Squares::C6;
        expected_incoming[58] |= Squares::A8;
        expected_incoming[59] |= Squares::C6;

        for s in 0..64 {
            assert_eq!(
                state.outgoing_attacks_by_square(s),
                expected_outgoing[s as usize]
            );
            assert_eq!(state.incoming_attacks(s), expected_incoming[s as usize]);
        }
    }

    #[test]
    fn attacks_after_promotion() {
        let fen = "rnbqkbnr/ppppp1pP/8/8/5pp1/8/PPPPPP2/RNBQKBNR w KQkq - 0 6";
        let mut state = GameState::from_fen(fen.into()).unwrap();
        let moves = legal_moves(&state, &Color::WHITE);

        let promotion = mv!(H7, G8);
        assert!(moves.contains(&promotion));
        state.make_move(&promotion, &Color::WHITE, &Piece::PAWN);

        assert_eq!(
            state.piece_bitboard(&Piece::QUEEN),
            Bitboard(0x4800000000000008)
        );
    }
}
