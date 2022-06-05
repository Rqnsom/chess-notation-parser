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

    #[test]
    fn insufficient_turn_length_failure() {
        assert_eq!(Turn::try_from(""), Err("Empty string received"));
        assert_eq!(Turn::try_from("a"), Err("Insufficient length"));
        assert_eq!(Turn::try_from("1"), Err("Insufficient length"));
    }

    #[test]
    fn turn_with_invalid_characters_failure() {
        let err = Err("Turn contains invalid characters");

        // All allowed characters, so we expect different failure here ;-)
        assert_ne!(err, Turn::try_from("abcdefgh12345678#+=x?!KQRNBP"));

        ["D1", "a0", "test", "P9", "A1.", "Yxh8", "v54"]
            .iter()
            .for_each(|&turn| {
                assert_eq!(err, Turn::try_from(turn));
            })
    }

    #[test]
    #[should_panic]
    fn castling_with_invalid_flags_will_panic() {
        let castling = turn_castling!(CastlingType::Short, 0xff);
        let _printable_string = castling.to_string();
    }

    #[test]
    fn castling_parsing_fails() {
        let err = Err("Unknown chess notation found");

        ["0-", "O--", "0-x", "O-o-O", "o-o", "-d4", "abcd-1234"]
            .iter()
            .for_each(|&castling| {
                assert_eq!(err, Turn::try_from(castling));
            })
    }

    #[test]
    fn trim_turn_str_success() {
        let arr = ['d', '4', '\0'].iter().collect::<String>();
        assert!(Turn::try_from(arr.as_str()).is_ok());

        let arr = ['d', '4', '\0', '\0'].iter().collect::<String>();
        assert!(Turn::try_from(arr.as_str()).is_ok());
    }

    #[test]
    fn king_does_his_castling_thing_happily() {
        let c_test_castling = |castling_type, castling_str: &[&str]| {
            castling_str.into_iter().for_each(|castling| {
                assert_eq!(castling_type, Turn::try_from(*castling).unwrap());
            });
        };

        use crate::CastlingType::*;
        c_test_castling(turn_castling!(Short), &["0-0", "O-O"]);
        c_test_castling(turn_castling!(Long), &["0-0-0", "O-O-O"]);
        c_test_castling(turn_castling!(Short, Flag::CHECK), &["O-O+", "0-0+"]);
        c_test_castling(
            turn_castling!(Long, Flag::CHECKMATE),
            &["0-0-0#", "O-O-O#"],
        );
    }

    #[test]
    fn pawns_move_forward() {
        // Create all possible turns from below chars and check that each
        // turn is a pawn turn and is not a promotion
        "234567"
            .chars()
            .map(|rank| Square::get_rank(rank).unwrap())
            .collect::<Vec<Vec<Square>>>()
            .into_iter()
            .flatten()
            .collect::<Vec<Square>>()
            .iter()
            .for_each(|square| {
                assert_eq!(
                    Turn::try_from(square.to_string().as_str()).unwrap(),
                    turn_move!(Piece::Pawn, *square)
                );
            });
    }

    #[test]
    fn pawns_must_be_promoted_on_the_final_rank() {
        "18".chars()
            .map(|rank| Square::get_rank(rank).unwrap())
            .collect::<Vec<Vec<Square>>>()
            .into_iter()
            .flatten()
            .collect::<Vec<Square>>()
            .iter()
            .for_each(|square| {
                assert_eq!(
                    Turn::try_from(square.to_string().as_str()),
                    Err("Pawn must be promoted on the final rank")
                );
            });
    }

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

    #[test]
    fn pawns_are_promoted_to_invalid_pieces() {
        let invalid_piece = "a1=K h8=P b1=K c1=P";
        let invalid_square = "b2=B a3=Q h4=N c5=R f6=Q d7=N";

        for turn in invalid_piece.split_whitespace() {
            assert_eq!(Err("Invalid promotion piece"), Turn::try_from(turn));
        }

        for turn in invalid_square.split_whitespace() {
            assert_eq!(Err("Invalid promotion square"), Turn::try_from(turn));
        }
    }

    #[test]
    fn turn_is_checking() {
        let moves = "e4 c5 Nf3 d6 d4 cxd4 Nxd4 Nf6 Nc3 a6 h3 Nc6
            g4 Nxd4 Qxd4 e5 Qd3 0-0 Be7 g5 Nd7 Be3 Nc5 Qd2 Be6";

        for turn in moves.split_whitespace() {
            // No 'check' here
            assert!(!Turn::try_from(turn).unwrap().is_check());
        }

        let moves = "e4+ c5+ Nf3+ d6+ d4+ cxd4+ Nxd4+ Nf6+ Nc3+ a6+ h3+ O-O+";
        for turn in moves.split_ascii_whitespace() {
            assert!(Turn::try_from(turn)
                .expect(format!("\"{}\" should not fail!", turn).as_str())
                .is_check());
        }
    }

    #[test]
    fn turn_is_checkmating() {
        let moves = "e4 c5 Nf3 d6 d4 cxd4 Nxd4 Nf6 Nc3 a6 h3 Nc6
            g4 Nxd4 Qxd4 e5 Qd3 Be7 g5 Nd7 Be3 Nc5 Qd2 Be6 0-0";

        for turn in moves.split_whitespace() {
            // No 'checkmate' here
            assert!(!Turn::try_from(turn).unwrap().is_checkmate());
        }

        let moves = "e4# c5# Nf3# d6# d4# cxd4# Nxd4# Nf6# Nc3# a6# h3# 0-0#";
        for turn in moves.split_ascii_whitespace() {
            assert!(Turn::try_from(turn)
                .expect(format!("\"{}\" should not fail!", turn).as_str())
                .is_checkmate());
        }
    }

    #[test]
    fn turn_is_capturing() {
        let non_capturing_moves = "e4 c5 Nf3 d6 d4 Nf6 Nc3 a6 h3 Nc6
            g4 e5 Qd3 Be7 g5 Nd7 Be3 Nc5 Qd2 Be6";

        for turn in non_capturing_moves.split_whitespace() {
            // No 'capture' here
            assert!(!Turn::try_from(turn).unwrap().is_checkmate());
        }

        let moves = "cxd4 Nxd4 Nxd4 Qxd4 Bxe3 Ne4xc5 Qxd2 Bxe6";
        for turn in moves.split_ascii_whitespace() {
            assert!(Turn::try_from(turn)
                .expect(format!("\"{}\" should not fail!", turn).as_str())
                .is_capture());
        }
    }

    #[test]
    fn dst_square_not_found() {
        let turns = "=+ R+ =R =d d= += +# #3 3# ab 12 8a+ P1 Bh B1g Kge Kg1e";

        for turn in turns.split_whitespace() {
            if let Err(msg) = Turn::try_from(turn) {
                assert_eq!("Destination square not found", msg);
            }
        }
    }

    #[test]
    fn source_square_is_empty() {
        let source_move_empty = "e4 c5 Nf3 d6 d4 Nxd4 Nf6 Nc3 a6 h3 Nc6
            g4 Nxd4 Qxd4 e5 Qd3 Be7 g5 Nd7 Be3 Nc5 Qd2 Be6";

        for turn in source_move_empty.split_whitespace() {
            let turn = Turn::try_from(turn).unwrap();

            if let Turn::Move(Move { src, .. }) = turn {
                assert!(src.is_none());
            }
        }
    }

    #[test]
    fn source_squares_are_ranks_and_files() {
        let source_moves_from_rank_or_files = [
            ('d', "de4"),  // Some of these
            ('b', "bc5"),  // might not be
            ('e', "Nef3"), // even a valid chess
            ('c', "cd6"),  // moves, but it
            ('3', "3d4"),  // doesn't really
            ('c', "cxd4"), // matter for the
            ('f', "Nfd4"), // purpose of this
            ('d', "Ndf6"), // test
            ('d', "Ndc3"),
            ('b', "ba6"),
            ('g', "gh3"),
            ('b', "Nbc6"),
            ('f', "fg4"),
            ('c', "Ncxd4"),
            ('d', "de5"),
            ('d', "Bde7"),
            ('h', "hg5"),
            ('e', "Ned7"),
            ('d', "Bde3"),
            ('b', "Nbc5"),
            ('5', "B5e6"),
        ];

        for (rf, turn) in source_moves_from_rank_or_files.iter() {
            let rank_or_file = match rf.is_numeric() {
                true => Square::get_rank(*rf).unwrap(),
                false => Square::get_file(*rf).unwrap(),
            };

            let turn = Turn::try_from(*turn).unwrap();
            if let Turn::Move(Move { src: Some(src), .. }) = turn {
                assert_eq!(src, rank_or_file);
            }
        }
    }

    #[test]
    fn source_squares_is_specific_square() {
        let source_moves_from_rank_or_files = [
            (Square::D5, "d5e4"),  // Some of these
            (Square::B5, "b5c5"),  // might not be
            (Square::E2, "Ne2f3"), // even a valid chess
            (Square::C2, "c2d6"),  // moves, but it
            (Square::G3, "g3d4"),  // doesn't really
            (Square::C3, "c3xd4"), // matter for the
            (Square::F6, "Nf6d4"), // purpose of this
            (Square::D5, "Nd5f6"), // test
            (Square::D2, "Nd2c3"),
            (Square::B3, "b3a6"),
            (Square::D8, "Bd8e7"),
            (Square::H2, "h2g5"),
            (Square::E7, "Re7d7"),
            (Square::D3, "Kd3e3"),
            (Square::B1, "Nb1c5"),
            (Square::H5, "Bh5e6"),
        ];

        for (square, turn) in source_moves_from_rank_or_files.iter() {
            let turn = Turn::try_from(*turn).unwrap();

            if let Turn::Move(Move {
                src: Some(mut src), ..
            }) = turn
            {
                assert!(src.len() == 1);
                assert_eq!(src.pop().unwrap(), *square);
            }
        }
    }

    #[test]
    #[should_panic(expected = "'Source' should contain a set of 8 ranks/files")]
    fn source_square_has_incomplete_rank() {
        let _invalid_turn = turn_move!(
            Piece::Rook,
            Square::A8,
            Flag::NONE,
            Some(vec![Square::B1, Square::B2]),
            None
        );

        _invalid_turn.to_string();
    }

    #[test]
    fn comment_syntax_invalid() {
        let turns = ["d5e4!!!", "b5c5???", "!Ne2f3+", "?c2xd6", "g3d4?!?"];

        for turn in turns.into_iter() {
            assert_eq!(Err("Invalid comment syntax"), Turn::try_from(turn));
        }
    }

    #[test]
    fn comment_syntax_valid() {
        let turns = [
            ("d5e4!", "d5e4"),    // Some of these might
            ("b5c5!!", "b5c5"),   // not be even a valid
            ("Ne2f3?", "Ne2f3"),  // chess moves, but it
            ("c2d6??", "c2d6"),   // doesn't really matter
            ("g3d4!?", "g3d4"),   // matter for the purpose
            ("c3xd4?!", "c3xd4"), // of this test
        ];

        for (comment_move, clean_move) in turns.into_iter() {
            assert_eq!(
                clean_move,
                Turn::try_from(comment_move).unwrap().to_string()
            );
        }
    }

    #[test]
    fn turns_with_invalid_and_silly_format_shall_fail() {
        let turns = "d5a5e4 0-0-0-0 0-0-! Pc2xd6 g3dd4 RQd4 a1Qh2 a2xb3+# \
            a1==Q #+a5 1axb2 Na1=Q O-O-0+ Rb1a2c3 Nabc3 Raaa2";

        for turn in turns.split_whitespace() {
            assert_eq!(
                Err("Turn verification failed: invalid format"),
                Turn::try_from(turn)
            );
        }
    }

    #[test]
    fn test_all_turn_components() {
        assert_eq!(
            Turn::try_from("0-0-0#?").unwrap(),
            turn_castling!(CastlingType::Long, Flag::CHECKMATE),
        );

        assert_eq!(
            Turn::try_from("Re7d7!!").unwrap(),
            turn_move!(
                Piece::Rook,
                Square::D7,
                Flag::NONE,
                Some(vec![Square::E7])
            ),
        );

        assert_eq!(
            Turn::try_from("Qxd2#").unwrap(),
            turn_move!(
                Piece::Queen,
                Square::D2,
                Flag::CAPTURE | Flag::CHECKMATE
            ),
        );

        assert_eq!(
            Turn::try_from("Naxb3+?").unwrap(),
            turn_move!(
                Piece::Knight,
                Square::B3,
                Flag::CAPTURE | Flag::CHECK,
                Some(Square::get_file('a').unwrap())
            ),
        );

        assert_eq!(
            Turn::try_from("a2xb1=B").unwrap(),
            turn_move!(
                Piece::Pawn,
                Square::B1,
                Flag::CAPTURE,
                Some(vec![Square::A2]),
                Some(Piece::Bishop)
            ),
        );
    }
}
