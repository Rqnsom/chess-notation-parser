use chess_notation_parser::*;

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
