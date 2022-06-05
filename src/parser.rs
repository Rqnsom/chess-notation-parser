//! A helper parser for turn enum

use crate::{
    turn_move, Castling, CastlingType, Flag, Move, Piece, Square, Turn,
};

// Fill out `Castling` struct from the turn input
pub fn parse_castling(castling: &str) -> Result<Turn, &'static str> {
    let flags = get_flags(castling);

    let r#type = if castling.contains("0-0-0") || castling.contains("O-O-O") {
        CastlingType::Long
    } else if castling.contains("0-0") || castling.contains("O-O") {
        CastlingType::Short
    } else {
        return Err("Unknown chess notation found");
    };

    Ok(Turn::Castling(Castling { r#type, flags }))
}

// Get piece character for each Piece according
// to chess notation rules in English language
pub fn get_piece_char(piece: Piece) -> char {
    match piece {
        Piece::King => 'K',
        Piece::Queen => 'Q',
        Piece::Rook => 'R',
        Piece::Knight => 'N',
        _ => 'B', // Ignore 'never-annotated' pawns
    }
}

// To be used on slices with ASCII strings
unsafe fn get_char_from_idx(slice: &str, idx: usize) -> char {
    *slice.as_ptr().offset(idx.try_into().unwrap()) as char
}

// Make sure promotion makes sense
fn verify_promotion(piece: Piece, dst: Square) -> Result<(), &'static str> {
    match piece {
        Piece::King | Piece::Pawn => return Err("Invalid promotion piece"),
        _ => (),
    }

    match Square::get_rank('8').unwrap().contains(&dst)
        || Square::get_rank('1').unwrap().contains(&dst)
    {
        true => Ok(()),
        _ => Err("Invalid promotion square"),
    }
}

// Make sure promotion is not missing
fn verify_no_promotion(piece: Piece, dst: Square) -> Result<(), &'static str> {
    match piece {
        Piece::Pawn
            if Square::get_rank('8').unwrap().contains(&dst)
                || Square::get_rank('1').unwrap().contains(&dst) =>
        {
            Err("Pawn must be promoted on the final rank")
        }
        _ => Ok(()),
    }
}

fn get_piece_and_promotion(turn_move: &str) -> (Piece, Option<Piece>) {
    // Only pieces can be upper case
    let piece_idx = match turn_move.find(char::is_uppercase) {
        None => return (Piece::Pawn, None),
        Some(idx) => idx,
    };

    let piece_char = unsafe { get_char_from_idx(turn_move, piece_idx) };
    let piece = match piece_char {
        'K' => Piece::King,
        'Q' => Piece::Queen,
        'R' => Piece::Rook,
        'N' => Piece::Knight,
        'B' => Piece::Bishop,
        _ => Piece::Pawn,
    };

    match turn_move.contains('=') {
        false => (piece, None),
        true => (Piece::Pawn, Some(piece)),
    }
}

fn get_flags(turn: &str) -> u8 {
    let mut flags: u8 = 0x00;

    if turn.contains('x') {
        flags |= Flag::CAPTURE;
    }

    // Can't be both at the same time
    if turn.contains('+') {
        flags |= Flag::CHECK;
    } else if turn.contains('#') {
        flags |= Flag::CHECKMATE;
    }

    flags
}

// First find the destination square
// Then, try to see if we can find any source square.
fn get_squares(
    turn_move: &str,
) -> Result<(Square, Option<Vec<Square>>), &'static str> {
    const SQUARE_CHARS: &[char] = &[
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', '1', '2', '3', '4', '5', '6',
        '7', '8',
    ];
    const STR_DST_SQR_NOT_FOUND: &str = "Destination square not found";

    // Going backwards must provide us with a dst square!
    let (dst_idx_start, dst_idx_end) = match turn_move.rfind(SQUARE_CHARS) {
        Some(i) if i > 0 => (i - 1, i),
        _ => return Err(STR_DST_SQR_NOT_FOUND),
    };
    // Hopefully the compiler will optimize this
    let dst_idx_range = dst_idx_start..=dst_idx_end;
    let dst_square = Square::try_from(&turn_move[dst_idx_range])
        .map_err(|_| STR_DST_SQR_NOT_FOUND)?;

    // Now going foward
    let src_idx_start = turn_move.find(SQUARE_CHARS).unwrap();
    if src_idx_start == dst_idx_start {
        return Ok((dst_square, None));
    }

    let src_idx_range = src_idx_start..=src_idx_start + 1;

    let src_squares = Some(match Square::try_from(&turn_move[src_idx_range]) {
        // Ok means that a specific square has been found
        Ok(square) => vec![square],

        // Err means we are dealing only with a rank or a file char
        Err(_) => {
            // It's safe to index ASCII contained string slice
            let c = unsafe { get_char_from_idx(turn_move, src_idx_start) };

            match c.is_numeric() {
                true => Square::get_rank(c)?,
                false => Square::get_file(c)?,
            }
        }
    });

    Ok((dst_square, src_squares))
}

// Fill out `Move` struct from the turn input
pub fn parse_move(turn_move: &str) -> Result<Turn, &'static str> {
    const ALLOWED_CHARS: &'static str = "abcdefgh12345678#+=x?!KQRNBP";

    // Any turns with dirty non-ASCII UTF-8 characters are dropped here
    for c in turn_move.chars() {
        if !ALLOWED_CHARS.contains(c) {
            return Err("Turn contains invalid characters");
        }
    }

    let flags = get_flags(turn_move);
    let (piece, promotion) = get_piece_and_promotion(turn_move);
    let (dst, src) = get_squares(turn_move)?;

    match promotion {
        Some(promotion) => verify_promotion(promotion, dst)?,
        None => verify_no_promotion(piece, dst)?,
    }

    Ok(turn_move!(piece, dst, flags, src, promotion))
}
