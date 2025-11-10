use bonk::{
    GameResult, Move, Piece,
    movegen::legal_moves,
    search::{is_game_over, search},
    state::GameState,
    util::square_to_algebraic,
};
use std::io::{self, Write};

fn algebraic_to_square(s: &str) -> Option<u8> {
    if s.len() != 2 {
        return None;
    }
    let chars: Vec<char> = s.chars().collect();
    let file = chars[0];
    let rank = chars[1];

    if !('a'..='h').contains(&file) || !('1'..='8').contains(&rank) {
        return None;
    }

    let file_idx = (file as u8) - b'a';
    let rank_idx = (rank as u8) - b'1';

    Some(rank_idx * 8 + file_idx)
}

fn parse_move(input: &str) -> Option<Move> {
    let trimmed = input.trim();
    if trimmed.len() < 4 {
        return None;
    }

    let start = algebraic_to_square(&trimmed[0..2])?;
    let end = algebraic_to_square(&trimmed[2..4])?;

    Some(Move { start, end })
}

fn display_legal_moves(moves: &[Move]) {
    print!("Legal moves: ");
    for (i, m) in moves.iter().enumerate() {
        print!(
            "{}{}",
            square_to_algebraic(m.start),
            square_to_algebraic(m.end)
        );
        if i < moves.len() - 1 {
            print!(", ");
        }
    }
    println!();
}

fn get_piece_name(piece: Piece) -> &'static str {
    match piece {
        Piece::PAWN => "pawn",
        Piece::KNIGHT => "knight",
        Piece::BISHOP => "bishop",
        Piece::ROOK => "rook",
        Piece::QUEEN => "queen",
        Piece::KING => "king",
    }
}

fn main() {
    println!("=== Bonk Chess Engine ===\n");
    println!("Enter FEN or press enter to start from new position");

    let mut fen = String::new();
    std::io::stdin().read_line(&mut fen).unwrap();
    fen = fen.trim().to_string();

    let mut state = if fen.is_empty() {
        GameState::default()
    } else {
        GameState::from_fen(fen).unwrap()
    };
    let mut move_number = 1;

    println!("Enter engine search depth (max 20):");
    let mut depth_input = String::new();
    std::io::stdin().read_line(&mut depth_input).unwrap();
    let depth: u8 = depth_input.trim_end().parse().unwrap();
    assert!(depth <= 20);

    let engine_color = state.to_move();

    loop {
        // Clear screen and show current position
        print!("\x1B[2J\x1B[1;1H"); // ANSI clear screen
        println!("Move {}\n", move_number);
        println!("{}", state);

        // Check for game over
        if let Some(result) = is_game_over(&state) {
            match result {
                GameResult::Checkmate(winner) => {
                    println!("\nCheckmate! {:?} wins!", winner);
                }
                GameResult::Stalemate => {
                    println!("\nStalemate! Draw.");
                }
                GameResult::FiftyMoveRule => {
                    println!("\nDraw by 50-move rule.");
                }
            }
            break;
        }

        let current_color = state.to_move();
        let moves = legal_moves(&state, current_color);

        if moves.is_empty() {
            println!("No legal moves!");
            break;
        }

        if current_color != engine_color {
            // Human's turn
            println!("Your turn");
            display_legal_moves(&moves);

            loop {
                print!("\nEnter move (e.g., e2e4) or 'quit': ");
                io::stdout().flush().unwrap();

                let mut input = String::new();
                io::stdin().read_line(&mut input).unwrap();
                let input = input.trim();

                if input == "quit" || input == "q" {
                    println!("Thanks for playing!");
                    return;
                }

                if let Some(user_move) = parse_move(input) {
                    if moves.contains(&user_move) {
                        let piece = state.piece_at(user_move.start).unwrap();
                        state.make_move(user_move, !engine_color, piece);
                        println!(
                            "\nYou moved {} from {} to {}",
                            get_piece_name(piece),
                            square_to_algebraic(user_move.start),
                            square_to_algebraic(user_move.end)
                        );
                        break;
                    } else {
                        println!("Illegal move! Try again.");
                    }
                } else {
                    println!("Invalid format! Use format like 'e2e4'");
                }
            }
        } else {
            // Engine's turn
            println!("Engine thinking (depth {})...", depth);

            if let Some((best_move, score)) = search(&state, depth) {
                let piece = state.piece_at(best_move.start).unwrap();
                println!(
                    "\nEngine moved {} from {} to {} (score: {:.2})",
                    get_piece_name(piece),
                    square_to_algebraic(best_move.start),
                    square_to_algebraic(best_move.end),
                    score
                );

                state.make_move(best_move, engine_color, piece);
                move_number += 1;

                println!("\nPress Enter to continue...");
                let mut dummy = String::new();
                io::stdin().read_line(&mut dummy).unwrap();
            } else {
                println!("Engine has no legal moves!");
                break;
            }
        }
    }
}
