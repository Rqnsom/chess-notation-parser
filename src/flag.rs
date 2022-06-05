//! Flag bitmask indicates special information about the turn

/// Bitmask flag depicts special capabilities of a chess turn (*check*,
/// *checkmate*, *capture*)
///
/// *Note:* `CHECK` and `CHECKMATE` are mutually exclusive flags.
#[allow(non_snake_case)]
pub mod Flag {
    /// Turn will result in a check
    pub const NONE: u8 = 0;

    /// Turn will result in a check
    pub const CHECK: u8 = 0x01 << 0;

    /// Turn will result in a checkmate
    pub const CHECKMATE: u8 = 0x01 << 1;

    /// Turn captures a piece of the opponent
    pub const CAPTURE: u8 = 0x01 << 2;
}

/// Implements function to easily check *chess turn* flags
pub trait FlagCheck {
    /// Getter function for `flags`
    fn get_flags(&self) -> u8;

    /// Returns true if flag is present
    ///
    /// # Example
    /// ```
    /// use chess_notation_parser::{Turn, Move, Square, Piece, Flag, FlagCheck};
    ///
    /// let r#move = Move {
    ///     who: Piece::King,
    ///     dst: Square::D5,
    ///     flags: Flag::CAPTURE,
    ///     src: None,
    ///     promotion: None
    /// };
    ///
    /// assert!(r#move.check_flag(Flag::CAPTURE));
    /// assert!(!r#move.check_flag(Flag::CHECK));
    /// ```
    fn check_flag(&self, flag: u8) -> bool {
        self.get_flags() & flag != 0
    }
}
