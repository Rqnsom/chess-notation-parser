# Chess notation parser
Crate transforms algebraic chess notation into software readable structs and
vice versa. Parsed chess notation for each turn is stored within `Turn`
struct.

To parse a certain chess turn, such as `d2xe3`, store it in form of `&str` and
pass it as an argument into `Turn::try_from()` function.

`Turn` is an enum with two elements:
- `Castling` - a struct which describes *castling* turn
- `Move` - a struct which describes every other possible turn

## Example for `Castling` turn
#### `0-0` will be translated to:
```rust
Turn::Castling(Castling {
    r#type: CastlingType::Short,
    flags: Flag::NONE,
});
```

## Examples for `Move` turns
#### `d6` will be translated to:
```rust
Turn::Move (Move {
    who: Piece::Pawn,
    dst: Square::D6,
    flags: Flag::NONE,
    src: None,
    promotion: None,
});
```

#### `d7xe8=B+?` will be translated to:
```rust
Turn::Move (Move {
    who: Piece::Pawn,
    dst: Square::E8,
    flags: Flag::CHECK | Flag::CAPTURE,
    src: Some(vec![Square::D7]),
    promotion: Some(Piece::Bishop),
});
```

#### `Nab3#` will be translated to:
```rust
Turn::Move (Move {
    who: Piece::Knight,
    dst: Square::B3,
    flags: Flag::CHECKMATE,
    src: Some(Square::get_file('a').unwrap()),  // Vector of 'Ax' squares
    promotion: None,
});
```

# Chess notation parser rules
- **Square notation** should use lowercase alphabetic characters
  - Valid: `a1`, `a2` ... `h7`, `h8`.

- **Castling notation** can be written with both `0` and `O`
  - Valid example: `0-0-0` or `O-O`
  - When `Castling` turn is printed out, it will be printed with `0`
  notation

- Notation for **pieces**:
  - `K`: King
  - `Q`: Queen
  - `R`: Rook
  - `B`: Bishop
  - `N`: Knight
  - Pawns are indicated by the absence of the letter

- **Capture** is annotated with a lowercase `x` character
  - Valid example: `Qxd3`

- **Check** is annotated with a `+` character
  - Valid example: `Qd3+`

- **Checkmate** is annotated with a `#` character
  - Valid example: `Qd3#`

- **Pawn promotion** is annoted with `=` symbol followed by a piece to which
  pawn is promoted to
  - Pawn promotion is valid only for ranks `8` and `1`
  - Valid example: `g8=Q`

- Comments `??`, `!!`, `?`, `!`, `!?`, `?!` are allowed only at the end of
the turn
  - Valid example: `a1=B??`
  - Invalid example: `??a1=B`

