//! Enum representation of a chess board square

use std::fmt;

/// # Chess board square
///
/// Square are identified with `<file><rank>` format:
/// - `file` - Lowercase alphabet character from range `[A-H]`.
///            Sometimes referred to as columns.
/// - `rank` - Numeric character from range `[1-8]`.
///            Sometimes referred to as rows.
///
/// Setup of white pieces is done on ranks `1` and `2`,
/// whilist setup of black pieces is done on ranks `7` and `8`.
///
/// ## Example
/// ```
/// use chess_notation_parser::Square;
///
/// // Creation of squares is done with <&str>try_from() function
/// assert_eq!(Ok(Square::A1), Square::try_from("a1"));
///
/// // Only lowercase letters should be used
/// assert_eq!(Err("Invalid square input"), Square::try_from("A1"));
///
/// // Invalid input is rejected
/// assert_eq!(Err("Invalid square input"), Square::try_from("A9"));
/// assert_eq!(Err("Invalid square input"), Square::try_from("K1"));
/// ```
#[derive(Debug, Hash, PartialOrd, Ord, PartialEq, Copy, Clone, Eq)]
#[rustfmt::skip]
pub enum Square {
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
}

const RANK_1: &[&str] = &["a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1"];
const RANK_2: &[&str] = &["a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2"];
const RANK_3: &[&str] = &["a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3"];
const RANK_4: &[&str] = &["a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4"];
const RANK_5: &[&str] = &["a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5"];
const RANK_6: &[&str] = &["a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6"];
const RANK_7: &[&str] = &["a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7"];
const RANK_8: &[&str] = &["a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"];

const FILE_A: &[&str] = &["a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"];
const FILE_B: &[&str] = &["b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8"];
const FILE_C: &[&str] = &["c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"];
const FILE_D: &[&str] = &["d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8"];
const FILE_E: &[&str] = &["e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"];
const FILE_F: &[&str] = &["f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8"];
const FILE_G: &[&str] = &["g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8"];
const FILE_H: &[&str] = &["h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8"];

impl Square {
    /// Get a set of squares representing certain file
    ///
    /// # Example
    /// ```
    /// use chess_notation_parser::Square;
    ///
    /// let file_a = Square::get_file('a').unwrap();
    /// let mut iter = file_a.into_iter();
    ///
    /// assert_eq!(Some(Square::A1), iter.next());
    /// assert_eq!(Some(Square::A2), iter.next());
    /// assert_eq!(Some(Square::A3), iter.next());
    /// assert_eq!(Some(Square::A4), iter.next());
    /// assert_eq!(Some(Square::A5), iter.next());
    /// assert_eq!(Some(Square::A6), iter.next());
    /// assert_eq!(Some(Square::A7), iter.next());
    /// assert_eq!(Some(Square::A8), iter.next());
    /// assert_eq!(None, iter.next());
    /// ```
    pub fn get_file(file: char) -> Result<Vec<Square>, &'static str> {
        let file = match file {
            'a' => FILE_A,
            'b' => FILE_B,
            'c' => FILE_C,
            'd' => FILE_D,
            'e' => FILE_E,
            'f' => FILE_F,
            'g' => FILE_G,
            'h' => FILE_H,
            _ => return Err("Invalid 'file' character received"),
        };

        Ok(file
            .iter() // We are safe to use unwrap if we reached this point
            .map(|square| Square::try_from(*square).unwrap())
            .collect::<Vec<Square>>())
    }

    /// Get a set of squares representing certain rank
    ///
    /// # Example
    /// ```
    /// use chess_notation_parser::Square;
    ///
    /// let rank_a = Square::get_rank('2').unwrap();
    /// let mut iter = rank_a.into_iter();
    ///
    /// assert_eq!(Some(Square::A2), iter.next());
    /// assert_eq!(Some(Square::B2), iter.next());
    /// assert_eq!(Some(Square::C2), iter.next());
    /// assert_eq!(Some(Square::D2), iter.next());
    /// assert_eq!(Some(Square::E2), iter.next());
    /// assert_eq!(Some(Square::F2), iter.next());
    /// assert_eq!(Some(Square::G2), iter.next());
    /// assert_eq!(Some(Square::H2), iter.next());
    /// assert_eq!(None, iter.next());
    /// ```
    pub fn get_rank(rank: char) -> Result<Vec<Square>, &'static str> {
        let rank = match rank {
            '8' => RANK_8,
            '7' => RANK_7,
            '5' => RANK_5,
            '6' => RANK_6,
            '4' => RANK_4,
            '3' => RANK_3,
            '2' => RANK_2,
            '1' => RANK_1,
            _ => return Err("Invalid 'rank' character received"),
        };

        Ok(rank
            .iter() // We are safe to use unwrap if we reached this point
            .map(|square| Square::try_from(*square).unwrap())
            .collect::<Vec<Square>>())
    }

    /// Get relative neighbor using relative `(x,y)` coordinates
    ///
    /// # Example
    /// ```
    /// use chess_notation_parser::Square;
    ///
    /// let d4 = Square::D4;
    ///
    /// let d2 = d4.get_relative_neighbor(0, -2).unwrap();
    /// assert_eq!(d2, Square::D2);
    ///
    /// let e5 = d4.get_relative_neighbor(1, 1).unwrap();
    /// assert_eq!(e5, Square::E5);
    /// ```
    pub fn get_relative_neighbor(
        &self,
        x: i8,
        y: i8,
    ) -> Result<Square, &'static str> {
        let dst = *self as i8;

        let dst_mod = dst % 8;
        let l_border = match dst_mod {
            0 => 0,
            dst_mod => -dst_mod,
        };
        let r_border = 8 - dst_mod;
        if x < l_border || x >= r_border {
            return Err("Square out of range");
        }

        let y = dst - y * 8;

        let result = x + y;
        if result < 0 || result > Square::H1 as i8 {
            return Err("Square out of range");
        }

        use std::mem::transmute;
        let dst: Self = unsafe { transmute(result as i8) };
        Ok(dst)
    }

    /// Get file char
    pub fn get_file_char(&self) -> char {
        use Square::*;
        match self {
            A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 => 'a',
            B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 => 'b',
            C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 => 'c',
            D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 => 'd',
            E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 => 'e',
            F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 => 'f',
            G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8 => 'g',
            H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 => 'h',
        }
    }

    /// Get rank char
    pub fn get_rank_char(&self) -> char {
        use Square::*;
        match self {
            A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 => '8',
            A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 => '7',
            A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 => '5',
            A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 => '6',
            A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 => '4',
            A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 => '3',
            A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 => '2',
            A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 => '1',
        }
    }
}

impl From<u8> for Square {
    fn from(square: u8) -> Self {
        // Now, some real and honest work done here
        #[rustfmt::skip]
        const SQUARES: [Square; 64] = [
            Square::A8, Square::B8, Square::C8, Square::D8,
            Square::E8, Square::F8, Square::G8, Square::H8,
            Square::A7, Square::B7, Square::C7, Square::D7,
            Square::E7, Square::F7, Square::G7, Square::H7,
            Square::A6, Square::B6, Square::C6, Square::D6,
            Square::E6, Square::F6, Square::G6, Square::H6,
            Square::A5, Square::B5, Square::C5, Square::D5,
            Square::E5, Square::F5, Square::G5, Square::H5,
            Square::A4, Square::B4, Square::C4, Square::D4,
            Square::E4, Square::F4, Square::G4, Square::H4,
            Square::A3, Square::B3, Square::C3, Square::D3,
            Square::E3, Square::F3, Square::G3, Square::H3,
            Square::A2, Square::B2, Square::C2, Square::D2,
            Square::E2, Square::F2, Square::G2, Square::H2,
            Square::A1, Square::B1, Square::C1, Square::D1,
            Square::E1, Square::F1, Square::G1, Square::H1,
        ];

        let i = square as usize;
        assert!(i < SQUARES.len(), "Square index={} is out of range", i);
        SQUARES[i]
    }
}

impl TryFrom<&str> for Square {
    type Error = &'static str;

    fn try_from(square: &str) -> Result<Self, Self::Error> {
        if square.len() != 2 {
            return Err("Invalid square length");
        }

        // Ain't this silly. But should be fast! Hopefully...
        match square {
            "a1" => Ok(Self::A1),
            "a2" => Ok(Self::A2),
            "a3" => Ok(Self::A3),
            "a4" => Ok(Self::A4),
            "a5" => Ok(Self::A5),
            "a6" => Ok(Self::A6),
            "a7" => Ok(Self::A7),
            "a8" => Ok(Self::A8),

            "b1" => Ok(Self::B1),
            "b2" => Ok(Self::B2),
            "b3" => Ok(Self::B3),
            "b4" => Ok(Self::B4),
            "b5" => Ok(Self::B5),
            "b6" => Ok(Self::B6),
            "b7" => Ok(Self::B7),
            "b8" => Ok(Self::B8),

            "c1" => Ok(Self::C1),
            "c2" => Ok(Self::C2),
            "c3" => Ok(Self::C3),
            "c4" => Ok(Self::C4),
            "c5" => Ok(Self::C5),
            "c6" => Ok(Self::C6),
            "c7" => Ok(Self::C7),
            "c8" => Ok(Self::C8),

            "d1" => Ok(Self::D1),
            "d2" => Ok(Self::D2),
            "d3" => Ok(Self::D3),
            "d4" => Ok(Self::D4),
            "d5" => Ok(Self::D5),
            "d6" => Ok(Self::D6),
            "d7" => Ok(Self::D7),
            "d8" => Ok(Self::D8),

            "e1" => Ok(Self::E1),
            "e2" => Ok(Self::E2),
            "e3" => Ok(Self::E3),
            "e4" => Ok(Self::E4),
            "e5" => Ok(Self::E5),
            "e6" => Ok(Self::E6),
            "e7" => Ok(Self::E7),
            "e8" => Ok(Self::E8),

            "f1" => Ok(Self::F1),
            "f2" => Ok(Self::F2),
            "f3" => Ok(Self::F3),
            "f4" => Ok(Self::F4),
            "f5" => Ok(Self::F5),
            "f6" => Ok(Self::F6),
            "f7" => Ok(Self::F7),
            "f8" => Ok(Self::F8),

            "g1" => Ok(Self::G1),
            "g2" => Ok(Self::G2),
            "g3" => Ok(Self::G3),
            "g4" => Ok(Self::G4),
            "g5" => Ok(Self::G5),
            "g6" => Ok(Self::G6),
            "g7" => Ok(Self::G7),
            "g8" => Ok(Self::G8),

            "h1" => Ok(Self::H1),
            "h2" => Ok(Self::H2),
            "h3" => Ok(Self::H3),
            "h4" => Ok(Self::H4),
            "h5" => Ok(Self::H5),
            "h6" => Ok(Self::H6),
            "h7" => Ok(Self::H7),
            "h8" => Ok(Self::H8),

            _ => Err("Invalid square input"),
        }
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.get_file_char(), self.get_rank_char())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_from_invalid_length() {
        let err = Err("Invalid square length");

        assert_eq!(err, Square::try_from("h"));
        assert_eq!(err, Square::try_from("a10"));
    }

    #[test]
    fn get_rank_with_wrong_char_failure() {
        let err = Err("Invalid 'rank' character received");

        // We allow only lowercase letters
        assert_eq!(err, Square::get_rank('9'));

        // Garbage letter
        assert_eq!(err, Square::get_rank('z'));
    }

    #[test]
    fn get_file_with_wrong_char_failure() {
        let err = Err("Invalid 'file' character received");

        // We allow only lowercase letters
        assert_eq!(err, Square::get_file('A'));

        // Garbage letter
        assert_eq!(err, Square::get_file('z'));
    }

    #[test]
    #[should_panic]
    fn from_u8() {
        // The first enum member
        let square = Square::A8;
        assert_eq!(square as u8, 0);
        assert_eq!(square, Square::from(square as u8));

        // The last enum member
        let square = Square::H1;
        assert_eq!(square as u8, 63);
        assert_eq!(square, Square::from(square as u8));

        // Any integer after the last enum member must panic if given to
        // `from` function
        let _panic = Square::from(Square::H1 as u8 + 1);
    }

    #[test]
    fn get_rank_success() {
        let rank_str_char_pairs = [
            (RANK_1, '1'),
            (RANK_2, '2'),
            (RANK_3, '3'),
            (RANK_4, '4'),
            (RANK_5, '5'),
            (RANK_6, '6'),
            (RANK_7, '7'),
            (RANK_8, '8'),
        ];

        let c_test_rank_pair = |rank_strings: &[&str], rank_char| {
            let rank: Vec<Square> = rank_strings
                .iter()
                .map(|square| Square::try_from(*square).unwrap())
                .collect();
            assert_eq!(rank, Square::get_rank(rank_char).unwrap());
        };

        rank_str_char_pairs
            .iter()
            .for_each(|(rank_strings, rank_char)| {
                c_test_rank_pair(rank_strings, *rank_char);
            });
    }

    #[test]
    fn get_file_success() {
        let file_str_char_pairs = [
            (FILE_A, 'a'),
            (FILE_B, 'b'),
            (FILE_C, 'c'),
            (FILE_D, 'd'),
            (FILE_E, 'e'),
            (FILE_F, 'f'),
            (FILE_G, 'g'),
            (FILE_H, 'h'),
        ];

        let c_test_file_pair = |file_strings: &[&str], file_char| {
            let file: Vec<Square> = file_strings
                .iter()
                .map(|square| Square::try_from(*square).unwrap())
                .collect();
            assert_eq!(file, Square::get_file(file_char).unwrap());
        };

        file_str_char_pairs
            .iter()
            .for_each(|(file_strings, file_char)| {
                c_test_file_pair(file_strings, *file_char);
            });
    }

    #[test]
    fn get_relative_neighbor_suceess() {
        let square = Square::A1;
        assert_eq!(square.get_relative_neighbor(0, 0), Ok(Square::A1));
        assert_eq!(square.get_relative_neighbor(1, 0), Ok(Square::B1));
        assert_eq!(square.get_relative_neighbor(0, 1), Ok(Square::A2));
        assert_eq!(square.get_relative_neighbor(1, 1), Ok(Square::B2));
        assert_eq!(square.get_relative_neighbor(2, 2), Ok(Square::C3));
        assert_eq!(square.get_relative_neighbor(4, 4), Ok(Square::E5));
        assert_eq!(square.get_relative_neighbor(7, 7), Ok(Square::H8));

        let square1 = Square::D4;
        let square2 = Square::E5;
        assert_eq!(
            square1.get_relative_neighbor(1, 0),
            square2.get_relative_neighbor(0, -1)
        );

        let square = Square::H8;
        assert_eq!(square.get_relative_neighbor(-1, 0), Ok(Square::G8));
        assert_eq!(square.get_relative_neighbor(0, -1), Ok(Square::H7));
        assert_eq!(square.get_relative_neighbor(-1, -1), Ok(Square::G7));
        assert_eq!(square.get_relative_neighbor(-3, -7), Ok(Square::E1));
    }

    #[test]
    fn get_relative_neighbor_failure() {
        for square in Square::get_rank('1').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(0, -1),
                Err("Square out of range")
            );
        }
        for square in Square::get_rank('8').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(0, 1),
                Err("Square out of range")
            );
        }
        for square in Square::get_rank('7').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(0, 3),
                Err("Square out of range")
            );
        }

        for square in Square::get_file('a').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(-1, 0),
                Err("Square out of range")
            );
        }
        for square in Square::get_file('h').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(1, 0),
                Err("Square out of range")
            );
        }
        for square in Square::get_file('g').unwrap().into_iter() {
            assert_eq!(
                square.get_relative_neighbor(2, 0),
                Err("Square out of range")
            );
        }
    }
}
