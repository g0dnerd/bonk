use bonk::state::GameState;

fn main() {
    let fen = "r3k2r/pppq1ppp/2npbn2/2b1p3/2B1P3/2NPBN2/PPPQ1PPP/R3K2R w KQkq - 4 8";
    let state = GameState::from_fen(fen.into()).unwrap();
    println!("{state}");
}
