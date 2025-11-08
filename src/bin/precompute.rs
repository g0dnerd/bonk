use std::{
    fs::File,
    io::{BufWriter, Error, Write},
};

use bonk::{
    BISHOP_DIRECTIONS, MagicTableEntry, ROOK_DIRECTIONS, SliderDirections, Square,
    bitboard::Bitboard, movegen::magic_tbl_idx, try_square_offset,
};
use rand::{Rng, rng};

pub fn main() -> std::io::Result<()> {
    let mut rng = rng();
    precompute_magics(&mut rng)?;

    let rook_tbl = make_move_table(
        bonk::movegen::magics::ROOK_TABLE_SIZE,
        &ROOK_DIRECTIONS,
        bonk::movegen::magics::ROOK_MAGICS,
    );
    let bishop_tbl = make_move_table(
        bonk::movegen::magics::BISHOP_TABLE_SIZE,
        &BISHOP_DIRECTIONS,
        bonk::movegen::magics::BISHOP_MAGICS,
    );

    let mut out: std::path::PathBuf = "".into();
    out.push("src/movegen/");
    out.push("moves.rs");
    std::fs::remove_file(&out).ok();
    let mut out = BufWriter::new(File::create(out).unwrap());

    writeln!(out, "use crate::MagicTableEntry;")?;

    write_magics("ROOK", bonk::movegen::magics::ROOK_MAGICS, &mut out).unwrap();
    write_magics("BISHOP", bonk::movegen::magics::BISHOP_MAGICS, &mut out).unwrap();
    write_move_table("ROOK", &rook_tbl, &mut out).unwrap();
    write_move_table("BISHOP", &bishop_tbl, &mut out).unwrap();
    Ok(())
}

pub struct MagicEntry {
    magic: u64,
    mask: Bitboard,
    shift: u8,
}

fn slider_moves(s: Square, blockers: Bitboard, directions: &SliderDirections) -> Bitboard {
    let mut moves = Bitboard(0);

    for &(dx, dy) in directions {
        let mut ray = s;

        while !blockers.contains(ray) {
            if let Some(offs) = try_square_offset(ray, dx, dy) {
                ray = offs;
                moves |= ray;
            } else {
                break;
            }
        }
    }
    moves
}

// Potential blocker squares for a slider regardless of position.
fn blockers_for_square(s: Square, directions: &SliderDirections) -> Bitboard {
    let mut blockers = Bitboard(0);

    for (dx, dy) in directions {
        let mut ray = s;
        while let Some(offs) = try_square_offset(ray, *dx, *dy) {
            blockers |= ray;
            ray = offs;
        }
    }
    blockers & !Bitboard::from_square(s)
}

fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers & entry.mask;
    let hash = blockers.0.wrapping_mul(entry.magic);
    (hash >> entry.shift) as usize
}

fn compute_magic(
    directions: &SliderDirections,
    s: Square,
    shift_amt: u8,
    r: &mut impl Rng,
) -> (MagicEntry, Vec<Bitboard>) {
    let blockers = blockers_for_square(s, directions);
    let shift = 64 - shift_amt;

    loop {
        let magic = r.next_u64() & r.next_u64() & r.next_u64();

        let entry = MagicEntry {
            magic,
            mask: blockers,
            shift,
        };

        if let Ok(magics) = attempt_magics(directions, s, &entry) {
            return (entry, magics);
        }
    }
}

struct TableFillError;

fn attempt_magics(
    directions: &SliderDirections,
    s: Square,
    entry: &MagicEntry,
) -> Result<Vec<Bitboard>, TableFillError> {
    let shift = 64 - entry.shift;

    let mut tbl = vec![Bitboard(0); 1 << shift];
    let mut blockers = Bitboard(0);

    loop {
        let moves = slider_moves(s, blockers, directions);
        let potential_entry = &mut tbl[magic_index(entry, blockers)];

        if potential_entry.is_empty() {
            *potential_entry = moves;
        } else if *potential_entry != moves {
            return Err(TableFillError);
        }

        blockers.0 = blockers.0.wrapping_sub(entry.mask.0) & entry.mask.0;
        if blockers.is_empty() {
            break;
        }
    }

    Ok(tbl)
}

fn precompute_magics(r: &mut impl Rng) -> Result<(), Error> {
    let path = "./src/movegen/magics.rs";
    std::fs::remove_file(path).ok();
    let mut out_file = File::create(path)?;
    writeln!(out_file, "use crate::MagicTableEntry;")?;
    writeln!(out_file)?;

    for slider in &[ROOK_DIRECTIONS, BISHOP_DIRECTIONS] {
        let piece = if slider == &ROOK_DIRECTIONS {
            "ROOK"
        } else {
            "BISHOP"
        };

        writeln!(out_file, "#[rustfmt::skip]")?;
        let line = format!("pub const {piece}_MAGICS: &[MagicTableEntry; 64] = &[");
        writeln!(out_file, "{line}")?;
        let mut tbl_len = 0;

        for s in 0..64 {
            let num_blockers = blockers_for_square(s, slider).count_ones() as u8;

            let (entry, magics) = compute_magic(slider, s, num_blockers, r);

            let line = format!(
                "    MagicTableEntry {{ mask: 0x{:016X}, magic: 0x{:016X}, shift: {}, offset: {} }},",
                entry.mask.0, entry.magic, entry.shift, tbl_len
            );
            writeln!(out_file, "{line}")?;
            tbl_len += magics.len();
        }

        writeln!(out_file, "];")?;
        let line = format!("pub const {piece}_TABLE_SIZE: usize = {tbl_len};");
        writeln!(out_file, "{line}")?;
        writeln!(out_file)?;
    }
    Ok(())
}

fn make_move_table(
    size: usize,
    directions: &SliderDirections,
    magics: &[MagicTableEntry; 64],
) -> Vec<Bitboard> {
    let mut tbl = vec![Bitboard(0); size];

    for (s, entry) in magics.iter().enumerate() {
        let mask = Bitboard(entry.mask);

        let mut blockers = Bitboard(0);
        loop {
            let moves = slider_moves(s as Square, blockers, directions);
            tbl[magic_tbl_idx(entry, blockers)] = moves;

            blockers.0 = blockers.0.wrapping_sub(mask.0) & mask.0;
            if blockers.is_empty() {
                break;
            }
        }
    }
    tbl
}

fn write_move_table(name: &str, tbl: &[Bitboard], out: &mut impl Write) -> std::io::Result<()> {
    writeln!(out, "pub const {name}_MOVES: &[u64; {}] = &[", tbl.len())?;
    for entry in tbl {
        writeln!(out, "    {},", entry.0)?;
    }
    write!(out, "];")
}

fn write_magics(
    name: &str,
    magics: &[MagicTableEntry; 64],
    out: &mut impl Write,
) -> std::io::Result<()> {
    writeln!(out)?;
    writeln!(out, "pub const {name}_MAGICS: &[MagicTableEntry; 64] = &[")?;
    for e in magics {
        writeln!(out, "    MagicTableEntry {{")?;
        writeln!(out, "        mask: {},", e.mask)?;
        writeln!(out, "        magic: {},", e.magic)?;
        writeln!(out, "        shift: {},", e.shift)?;
        writeln!(out, "        offset: {},", e.offset)?;
        writeln!(out, "    }},")?;
    }
    writeln!(out, "];")
}
