//! A simple Move representation - a turn which moves a piece on a chessboard

use crate::flag::{Flag, FlagCheck};
use crate::parser;
use crate::piece::Piece;
use crate::square::Square;
use std::fmt;

/// # Move turn
///
/// `Move` struct represents any chess turn in which piece is moved from *square
/// A* to *square B*, with some additional information.
///
/// `Move` can represent any chess turn expect for castling. Castling is
/// described separately within 'Castling' struct.
///
/// ## Example
/// ```
/// use chess_notation_parser::{Turn, Move, Square, Piece, Flag};
///
/// let turn = Turn::Move(
///     Move {
///         who: Piece::Pawn,
///         dst: Square::C3,
///         flags: Flag::NONE,
///         src: None,
///         promotion: None,
///     }
/// );
/// assert_eq!(turn, Turn::try_from("c3").unwrap());
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Move {
    /// The piece which is moving from *square A* to *square B*
    pub who: Piece,

    /// Destination square (*square B*)
    pub dst: Square,

    /// Extra bits of information stored in a bitmask about the turn, check
    /// `Flag` for more info
    pub flags: u8,

    /// Description of a *square A*
    /// - Can be a specific square
    /// - Can be a rank or a file. In that case it is a set of 8 squares
    /// - Can be `None` when `src` doesn't need to be provided
    pub src: Option<Vec<Square>>,

    /// The `Piece` to which pawns are promoted to
    pub promotion: Option<Piece>,
}

impl Move {
    fn constuct_src_string(src: &Vec<Square>) -> String {
        match src.len() {
            1 => return src[0].to_string(),
            8 => (),
            _ => panic!("'Source' should contain a set of 8 ranks/files"),
        }

        // Get rank/file chars for the first square
        let (file, rank) = {
            let s = src[0].to_string();
            let mut square_chars = s.chars();
            (square_chars.next().unwrap(), square_chars.next().unwrap())
        };

        // Return whatever rank/file the second square can match
        match src[1].to_string().contains(file) {
            true => String::from(file),
            false => String::from(rank),
        }
    }
}

impl FlagCheck for Move {
    fn get_flags(&self) -> u8 {
        self.flags
    }
}

impl fmt::Display for Move {
    // Stringification goes in this order:
    //   <who><src><capture><dst><promotion><check/checkmate>
    //
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This capacity should be more than enough
        let mut s = String::with_capacity(16);

        // Pawns are not annotated in chess notation
        if self.who != Piece::Pawn {
            s.push(parser::get_piece_char(self.who));
        }

        // Append source if any
        if let Some(ref src) = self.src {
            s.push_str(Self::constuct_src_string(src).as_str());
        }

        if self.check_flag(Flag::CAPTURE) {
            s.push('x');
        }

        s.push_str(self.dst.to_string().as_str());

        if let Some(promotion_piece) = self.promotion {
            s.push('=');
            s.push(parser::get_piece_char(promotion_piece));
        }

        if self.check_flag(Flag::CHECK) {
            s.push('+');
        } else if self.check_flag(Flag::CHECKMATE) {
            s.push('#');
        }
        write!(f, "{}", s)
    }
}
