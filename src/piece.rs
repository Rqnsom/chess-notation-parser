use std::fmt;

/// Chess pieces
#[derive(Ord, PartialOrd, Debug, PartialEq, Eq, Copy, Clone)]
pub enum Piece {
    King,
    Queen,
    Rook,
    Knight,
    Bishop,
    Pawn,
}

impl TryFrom<&str> for Piece {
    type Error = &'static str;

    fn try_from(piece: &str) -> Result<Self, Self::Error> {
        Ok(match piece {
            "K" => Self::King,
            "Q" => Self::Queen,
            "R" => Self::Rook,
            "B" => Self::Bishop,
            "N" => Self::Knight,
            "P" => Self::Pawn,
            _ => return Err("Invalid piece character"),
        })
    }
}

impl fmt::Display for Piece {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Self::King => "King",
            Self::Queen => "Queen",
            Self::Rook => "Rook",
            Self::Knight => "Knight",
            Self::Bishop => "Bishop",
            Self::Pawn => "Pawn",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_player() {
        assert_eq!("King", format!("{}", Piece::King));
        assert_eq!("Queen", format!("{}", Piece::Queen));
        assert_eq!("Rook", format!("{}", Piece::Rook));
        assert_eq!("Knight", format!("{}", Piece::Knight));
        assert_eq!("Bishop", format!("{}", Piece::Bishop));
        assert_eq!("Pawn", format!("{}", Piece::Pawn));
    }

    #[test]
    fn try_from_valid() {
        assert_eq!(Piece::try_from("K").unwrap(), Piece::King);
        assert_eq!(Piece::try_from("Q").unwrap(), Piece::Queen);
        assert_eq!(Piece::try_from("R").unwrap(), Piece::Rook);
        assert_eq!(Piece::try_from("N").unwrap(), Piece::Knight);
        assert_eq!(Piece::try_from("B").unwrap(), Piece::Bishop);
        assert_eq!(Piece::try_from("P").unwrap(), Piece::Pawn);
    }

    #[test]
    fn try_from_invalid() {
        assert!(Piece::try_from("").is_err());
        assert!(Piece::try_from("too big string").is_err());
        assert!(Piece::try_from("X").is_err());
    }
}
