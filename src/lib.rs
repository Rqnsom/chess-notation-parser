//! # Chess notation parser
//! Crate transforms algebraic chess notation into software readable structs and
//! vice versa. Parsed chess notation for each turn is stored within `Turn`
//! struct.
//!
//! To parse a certain chess turn, such as `d2xe3`, store it in form of `&str`
//! and pass it as an argument into `Turn::try_from()` function.
//!
//! `Turn` is an enum with two elements:
//! - `Castling` - a struct which describes *castling* turn
//! - `Move` - a struct which describes every other possible turn
//!
//! ## Example for `Castling` turn
//! #### `0-0` will be translated to:
//! ```
//! # use chess_notation_parser::{Turn, Castling, CastlingType, Flag};
//! # use chess_notation_parser::{turn_castling};
//! # let turn =
//! Turn::Castling(Castling {
//!     r#type: CastlingType::Short,
//!     flags: Flag::NONE,
//! });
//! # assert_eq!(turn, Turn::try_from("O-O").unwrap());
//! ```
//!
//! ## Examples for `Move` turns
//! #### `d6` will be translated to:
//! ```
//! # use chess_notation_parser::{Turn, Move, Square, Piece, Flag};
//! # let turn =
//! Turn::Move (Move {
//!     who: Piece::Pawn,
//!     dst: Square::D6,
//!     flags: Flag::NONE,
//!     src: None,
//!     promotion: None,
//! });
//! # assert_eq!(turn, Turn::try_from("d6").unwrap())
//! ```
//!
//! #### `d7xe8=B+?` will be translated to:
//! ```
//! # use chess_notation_parser::{Turn, Move, Square, Piece, Flag};
//! # let turn =
//! Turn::Move (Move {
//!     who: Piece::Pawn,
//!     dst: Square::E8,
//!     flags: Flag::CHECK | Flag::CAPTURE,
//!     src: Some(vec![Square::D7]),
//!     promotion: Some(Piece::Bishop),
//! });
//! # assert_eq!(turn, Turn::try_from("d7xe8=B+?").unwrap())
//! ```
//!
//! #### `Nab3#` will be translated to:
//! ```
//! # use chess_notation_parser::{Turn, Move, Square, Piece, Flag};
//! # let turn =
//! Turn::Move (Move {
//!     who: Piece::Knight,
//!     dst: Square::B3,
//!     flags: Flag::CHECKMATE,
//!     src: Some(Square::get_file('a').unwrap()),  // Vector of 'Ax' squares
//!     promotion: None,
//! });
//! # assert_eq!(turn, Turn::try_from("Nab3#").unwrap())
//! ```
//!
//! # Chess notation parser rules
//! - **Square notation** should use lowercase alphabetic characters
//!   - Valid: `a1`, `a2` ... `h7`, `h8`.
//!
//! - **Castling notation** can be written with both `0` and `O`
//!   - Valid example: `0-0-0` or `O-O`
//!   - When `Castling` turn is printed out, it will be printed with `0`
//!   notation
//!
//! - Notation for **pieces**:
//!   - `K`: King
//!   - `Q`: Queen
//!   - `R`: Rook
//!   - `B`: Bishop
//!   - `N`: Knight
//!   - Pawns are indicated by the absence of the letter
//!
//! - **Capture** is annotated with a lowercase `x` character
//!   - Valid example: `Qxd3`
//!
//! - **Check** is annotated with a `+` character
//!   - Valid example: `Qd3+`
//!
//! - **Checkmate** is annotated with a `#` character
//!   - Valid example: `Qd3#`
//!
//! - **Pawn promotion** is annoted with `=` symbol followed by a piece to which
//!   pawn is promoted to
//!   - Pawn promotion is valid only for ranks `8` and `1`
//!   - Valid example: `g8=Q`
//!
//! - Comments `??`, `!!`, `?`, `!`, `!?`, `?!` are allowed only at the end of
//! the turn
//!   - Valid example: `a1=B??`
//!   - Invalid example: `??a1=B`

use std::fmt;

mod square;
pub use crate::square::Square;

mod piece;
pub use crate::piece::Piece;

mod flag;
pub use crate::flag::{Flag, FlagCheck};

mod r#move;
pub use crate::r#move::Move;

mod castling;
pub use crate::castling::{Castling, CastlingType};

mod parser;

/// # Struct representation of a string formatted chess turn
///
/// Turn can be either `Move` or `Castling` turn.
///
/// ## Example for `Move`
/// ```
/// use chess_notation_parser::{Turn, Move, Square, Piece, Flag};
///
/// let turn = Turn::Move(
///     Move {
///         who: Piece::Queen,
///         dst: Square::D7,
///         flags: Flag::NONE,
///         src: None,
///         promotion: None,
///     }
/// );
/// assert_eq!(turn, Turn::try_from("Qd7").unwrap());
///
/// // Turn::Move can be created with macro
/// use chess_notation_parser::turn_move;
/// let turn = turn_move!(Piece::Bishop, Square::C4);
/// assert_eq!(turn, Turn::try_from("Bc4").unwrap());
///
/// let turn = turn_move!(
///     Piece::King,
///     Square::D5,
///     Flag::CAPTURE,
///     Some(vec![Square::E3])
/// );
/// assert_eq!(turn, Turn::try_from("Ke3xd5").unwrap());
/// ```
/// ## Example for `Castling`
/// ```
/// use chess_notation_parser::{Turn, Castling, CastlingType, Flag};
/// use chess_notation_parser::turn_castling;
///
/// let turn = turn_castling!(
///     CastlingType::Long,
///     Flag::NONE
/// );
///
/// assert_eq!(turn, Turn::try_from("0-0-0").unwrap());
/// ```
#[derive(PartialEq, Debug, Clone)]
pub enum Turn {
    Castling(Castling),
    Move(Move),
}

impl Turn {
    /// Check if `Turn` results in check
    pub fn is_check(&self) -> bool {
        self.check_flag(Flag::CHECK)
    }

    /// Check if `Turn` results in checkmate
    pub fn is_checkmate(&self) -> bool {
        self.check_flag(Flag::CHECKMATE)
    }

    /// Check if `Turn` results in capture of opponent's piece
    pub fn is_capture(&self) -> bool {
        self.check_flag(Flag::CAPTURE)
    }

    fn check_flag(&self, flag: u8) -> bool {
        match self {
            Self::Move(turn_move) => turn_move.check_flag(flag),
            Self::Castling(turn_move) => turn_move.check_flag(flag),
        }
    }

    #[inline(always)]
    fn strip_turn_comments(turn: &str) -> Result<&str, &'static str> {
        let comment_start_idx = turn.find(&['?', '!']);

        match comment_start_idx {
            Some(i) => {
                match &turn[i..] {
                    "?" | "??" | "?!" | "!?" | "!!" | "!" => (),
                    _ => return Err("Invalid comment syntax"),
                }

                Ok(&turn[..i])
            }
            None => Ok(turn),
        }
    }
}

impl fmt::Display for Turn {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
                Self::Castling(ref castling) => castling.to_string(),
                Self::Move(ref turn_move) => turn_move.to_string(),
            }
        )
    }
}

impl TryFrom<&str> for Turn {
    type Error = &'static str;

    fn try_from(turn: &str) -> Result<Self, Self::Error> {
        let turn = turn.trim_end_matches('\0');

        if turn.is_empty() {
            return Err("Empty string received");
        }

        if turn.len() == 1 {
            return Err("Insufficient length");
        }

        // '-' is present only in castling turns
        let result = match turn.contains('-') {
            true => parser::parse_castling(turn),
            false => parser::parse_move(turn),
        }?;

        let result_str = result.to_string();
        let result_str = result_str.as_str();
        let turn_without_comments = Self::strip_turn_comments(turn)?;

        if result_str != turn_without_comments
            && result_str.replace("0", "O").as_str() != turn_without_comments
        {
            return Err("Turn verification failed: invalid format");
        }

        Ok(result)
    }
}

/// Creates a `Turn::Move` from given number of arguments
///
/// # Example
///
/// ```rust
/// use chess_notation_parser::{turn_move, Turn, Move, Square, Piece, Flag};
///
/// let turn = Turn::Move(
///     Move {
///         who: Piece::Queen,
///         dst: Square::D7,
///         flags: Flag::NONE,
///         src: None,
///         promotion: None,
///     }
/// );
///
/// assert_eq!(turn, turn_move!(Piece::Queen, Square::D7));
/// assert_eq!(turn, turn_move!(Piece::Queen, Square::D7, Flag::NONE));
/// assert_eq!(turn, turn_move!(Piece::Queen, Square::D7, Flag::NONE, None));
/// assert_eq!(turn, turn_move!(
///     Piece::Queen,
///     Square::D7,
///     Flag::NONE,
///     None,
///     None)
/// );
/// ```
#[macro_export]
macro_rules! turn_move {
    ($who:expr, $dst:expr) => {
        turn_move!($who, $dst, Flag::NONE, None, None)
    };

    ($who:expr, $dst:expr, $flags:expr) => {
        turn_move!($who, $dst, $flags, None, None)
    };

    ($who:expr, $dst:expr, $flags:expr, $src:expr) => {
        turn_move!($who, $dst, $flags, $src, None)
    };

    ($who:expr, $dst:expr, $flags:expr, $src:expr, $promotion:expr) => {
        Turn::Move(Move {
            who: $who,
            dst: $dst,
            flags: $flags,
            src: $src,
            promotion: $promotion,
        })
    };
}

/// Creates a `Turn::Castling` from given number of arguments
///
/// # Example
///
/// ```rust
/// use chess_notation_parser::{Flag, Turn, Castling, CastlingType};
/// use chess_notation_parser::turn_castling;
///
/// let turn = Turn::Castling(
///     Castling {
///         r#type: CastlingType::Short,
///         flags: Flag::NONE,
///     }
/// );
///
/// assert_eq!(turn, turn_castling!(CastlingType::Short));
/// assert_eq!(turn, turn_castling!(CastlingType::Short, Flag::NONE));
/// ```
#[macro_export]
macro_rules! turn_castling {
    ($castling_type:expr) => {
        turn_castling!($castling_type, Flag::NONE)
    };

    ($castling_type:expr, $flags:expr) => {
        Turn::Castling(Castling {
            r#type: $castling_type,
            flags: $flags,
        })
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// This test was kept here since `parser` mod is private
    #[test]
    fn pawns_are_promoted() {
        let c_check_promotion_for_piece = |square: Square, piece| {
            let mut square_str = square.to_string();
            square_str.push('=');
            square_str.push(parser::get_piece_char(piece));

            assert_eq!(
                Turn::try_from(square_str.as_str()).unwrap(),
                turn_move!(Piece::Pawn, square, Flag::NONE, None, Some(piece))
            );
        };

        // Create all possible turns from below chars and append promotion
        // piece to it
        "18".chars()
            .map(|rank| Square::get_rank(rank).unwrap())
            .collect::<Vec<Vec<Square>>>()
            .into_iter()
            .flatten()
            .collect::<Vec<Square>>()
            .into_iter()
            .for_each(|square| {
                // This will cover all possibilities since squares are
                // adjacent to each other
                match square as u8 % 4 {
                    0 => c_check_promotion_for_piece(square, Piece::Queen),
                    1 => c_check_promotion_for_piece(square, Piece::Bishop),
                    2 => c_check_promotion_for_piece(square, Piece::Rook),
                    _ => c_check_promotion_for_piece(square, Piece::Knight),
                }
            });
    }
}
