//! A simple castling representation

use crate::flag::{Flag, FlagCheck};
use std::fmt;

/// Castling type (side)
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CastlingType {
    /// Sometimes referred to as *castling queenside*
    Long,

    /// Sometimes referred to as *castling kingside*
    Short,
}

impl CastlingType {
    /// Return opposite castling type
    pub fn opposite(&self) -> Self {
        match self {
            Self::Short => Self::Long,
            Self::Long => Self::Short,
        }
    }
}

impl fmt::Display for CastlingType {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
                Self::Long => "0-0-0",
                Self::Short => "0-0",
            }
        )
    }
}

/// # Castling turn
///
/// In chess, castling is a special move where a King can move two squares
/// either to the left (Kingside Castle) or right (Queenside Castle).  The rook
/// is then placed to the left or right of the King respectively.
///
/// ## Example
/// ```
/// use chess_notation_parser::{Turn, Castling, CastlingType, Flag};
///
/// let turn = Turn::Castling(
///     Castling {
///         r#type: CastlingType::Short,
///         flags: Flag::CHECK,
///     }
/// );
///
/// assert_eq!(turn, Turn::try_from("0-0+").unwrap());
/// ```
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Castling {
    /// Castling type (side)
    pub r#type: CastlingType,

    /// Extra bits of information stored in a bitmask about the turn, check
    /// `Flag` for more info
    pub flags: u8,
}

impl FlagCheck for Castling {
    fn get_flags(&self) -> u8 {
        self.flags
    }
}

impl fmt::Display for Castling {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.flags {
            Flag::NONE => write!(f, "{}", self.r#type),
            Flag::CHECK => write!(f, "{}+", self.r#type),
            Flag::CHECKMATE => write!(f, "{}#", self.r#type),
            _ => panic!("Invalid flag used for castling struct"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opposite_castling_type() {
        let short = CastlingType::Short;
        let long = CastlingType::Long;

        assert_eq!(short, long.opposite());
        assert_eq!(short, short.opposite().opposite());

        assert_eq!(long, short.opposite());
        assert_eq!(long, long.opposite().opposite());
    }
}
