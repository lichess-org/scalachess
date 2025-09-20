package chess

import cats.syntax.option.*
import chess.Square.*
import chess.format.{ Fen, FullFen }
import chess.variant.*

class VariantTest extends ChessTest:

  List(Standard, Chess960, ThreeCheck, KingOfTheHill, Crazyhouse).foreach: variant =>
    test(s"$variant two-step pawn advance with no check should be valid"):
      val position = FullFen("2r3k1/p2Q1pp1/1p5p/3p4/P7/KP6/2r5/8 b - - 1 36")
      val game = fenToGame(position, variant).playMoves(A7 -> A5).get
      assert(game.position.playable(true))

    test(
      s"$variant when previous move is a double pawn push and checker is not the pushed pawn or a sliding piece"
    ):
      val game1 = Fen
        .read(variant, FullFen("r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"))
        .get
      val game2 = Fen
        .read(variant, FullFen("r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4"))
        .get

      assertNot(game1.variant.valid(game1, true))
      assert(game1.variant.valid(game1, false))
      assertNot(game2.variant.valid(game2, true))
      assert(game2.variant.valid(game2, false))

    test(
      s"$variant when previous move is a double pawn push and the only checker is a rook but not discovered check"
    ):
      val game = Fen
        .read(variant, FullFen("1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4"))
        .get
      assertNot(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(
      s"$variant when previous move is a double pawn push and the only checker is a bishop but not discovered check"
    ):
      val game = Fen
        .read(variant, FullFen("2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
        .get
      assertNot(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(s"$variant when multiple checkers are aligned with the king"):
      val game = Fen
        .read(variant, FullFen("1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4"))
        .get
      assertNot(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(s"$variant when previous move is a double pawn push and the only checker is the pushed pawn"):
      val game = Fen
        .read(variant, FullFen("r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4"))
        .get
      assert(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(s"$variant when two checkers are not on the same rank, file or diagonal"):
      val game = Fen
        .read(variant, FullFen("rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4"))
        .get
      assert(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(
      s"$variant when previous move is a double pawn push and the only checker is a discovered rook check"
    ):
      val game = Fen
        .read(variant, FullFen("1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4"))
        .get
      assert(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(
      s"$variant when previous move is a double pawn push and the only checker is a discovered bishop check"
    ):
      val game = Fen
        .read(variant, FullFen("1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
        .get
      assert(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

    test(
      s"$variant when multiple steppers give check"
    ):
      val game = Fen
        .read(variant, FullFen("2kq1r2/5rq1/1N1N4/N3N3/1KN5/N3B3/1N1N4/8 b k - 0 1"))
        .get
      assertNot(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

  test("standard position pieces correctly"):
    assertEquals(
      Standard.initialPieces,
      Map(
        A1 -> (White - Rook),
        B1 -> (White - Knight),
        C1 -> (White - Bishop),
        D1 -> (White - Queen),
        E1 -> (White - King),
        F1 -> (White - Bishop),
        G1 -> (White - Knight),
        H1 -> (White - Rook),
        A2 -> (White - Pawn),
        B2 -> (White - Pawn),
        C2 -> (White - Pawn),
        D2 -> (White - Pawn),
        E2 -> (White - Pawn),
        F2 -> (White - Pawn),
        G2 -> (White - Pawn),
        H2 -> (White - Pawn),
        A7 -> (Black - Pawn),
        B7 -> (Black - Pawn),
        C7 -> (Black - Pawn),
        D7 -> (Black - Pawn),
        E7 -> (Black - Pawn),
        F7 -> (Black - Pawn),
        G7 -> (Black - Pawn),
        H7 -> (Black - Pawn),
        A8 -> (Black - Rook),
        B8 -> (Black - Knight),
        C8 -> (Black - Bishop),
        D8 -> (Black - Queen),
        E8 -> (Black - King),
        F8 -> (Black - Bishop),
        G8 -> (Black - Knight),
        H8 -> (Black - Rook)
      )
    )

  test("standard Identify insufficient mating material when called (bishop)."):
    val position = FullFen("krq5/bqqq4/qqr5/1qq5/8/8/8/3qB2K b - -")
    val game = fenToGame(position, Standard)
    assertEquals(game.position.materialImbalance, -91)
    assert(game.position.opponentHasInsufficientMaterial)

  test("standard Identify sufficient mating material when called (bishop)."):
    val position = FullFen("8/7B/K7/2b5/1k6/8/8/8 b - -")
    val game = fenToGame(position, Standard)
    assertEquals(game.position.materialImbalance, 0)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("standard Identify insufficient mating material when called (knight)."):
    val position = FullFen("8/3k4/2q5/8/8/K1N5/8/8 b - -")
    val game = fenToGame(position, Standard)
    assertEquals(game.position.materialImbalance, -6)
    assert(game.position.opponentHasInsufficientMaterial)

  test("chess960 position pieces correctly"):
    assertEquals(Chess960.initialPieces.get(A2), Some(White - Pawn))

  test("chess960 initialize the board with castling rights"):
    assertEquals(Chess960.initialPosition.history.castles, Castles.init)

  test("kingOfTheHill detect win"):
    val game = Game(
      """
  PPk
  K
  """.kingOfTheHill
    )
    assertNot(game.position.end)

  test("kingOfTheHill regular checkMate"):
    val game = Game(
      """
PP
K  r
""".kingOfTheHill
    )
    assert(game.position.end)
    assertEquals(game.position.winner, Some(Black))

  test("kingOfTheHill centered black king"):
    val sit = Game(
      """
   k

PP
   K
""".kingOfTheHill
    ).position
    assert(sit.end)
    assertEquals(sit.winner, Some(Black))

  test("kingOfTheHill initialize the board with castling rights"):
    assertEquals(KingOfTheHill.initialPosition.history.castles, Castles.init)

  test("threeCheck detect win"):
    assertNot(
      Game(
        """
PPk
K
""".threeCheck
      ).position.end
    )
  test("threeCheck regular checkMate"):
    val game = Game(
      """
PP
K  r
""".threeCheck
    )
    assert(game.position.end)
    assertEquals(game.position.winner, Some(Black))
  test("threeCheck 1 check"):
    val game = Game(ThreeCheck.initialPosition)
      .playMoves(
        E2 -> E4,
        E7 -> E6,
        D2 -> D4,
        F8 -> B4
      )
      .get
    assertNot(game.position.end)
  test("threeCheck 2 checks"):
    val game = Game(ThreeCheck.initialPosition)
      .playMoves(
        E2 -> E4,
        E7 -> E6,
        D2 -> D4,
        F8 -> B4,
        C2 -> C3,
        B4 -> C3
      )
      .get
    assertNot(game.position.end)
  test("threeCheck 3 checks"):
    val game = Game(ThreeCheck.initialPosition)
      .playMoves(
        E2 -> E4,
        E7 -> E6,
        D2 -> D4,
        F8 -> B4,
        C2 -> C3,
        B4 -> C3,
        B1 -> C3,
        D8 -> H4,
        A2 -> A3,
        H4 -> F2
      )
      .get
    assert(game.position.end)
    assertEquals(game.position.winner, Some(Black))

  test("threeCheck Not force a draw when there is insufficient mating material"):
    val position = FullFen("8/6K1/8/8/8/8/k6p/8 b - - 1 39")
    fenToGame(position, ThreeCheck)
      .playMove(Square.H2, Square.H1, Knight.some)
      .assertRight: game =>
        assertNot(game.position.end)

  test("threeCheck Force a draw when there are only kings remaining"):
    val position = FullFen("8/6K1/8/8/8/8/k7/8 b - -")
    val game = fenToGame(position, ThreeCheck)
    assert(game.position.end)
    assertEquals(game.position.status, Status.Draw.some)

  test("threeCheck initialize the board with castling rights"):
    assertEquals(KingOfTheHill.initialPosition.history.castles, Castles.init)

  test("racingKings call it stalemate when there is no legal move"):
    val position = FullFen("8/8/8/8/3K4/8/1k6/b7 b - - 5 3")
    val game = fenToGame(position, RacingKings)
    assert(game.position.end)
    assert(game.position.staleMate)

  test("racingKings should not draw because of insufficient material"):
    val position = FullFen("8/8/8/8/5K2/8/2k5/8 w - - 0 1")
    val game = fenToGame(position, RacingKings)
    assertNot(game.position.end)
    assertNot(game.position.staleMate)

  test("racingKings should recognize a king in the goal"):
    val position = FullFen("2K5/8/6k1/8/8/8/8/Q6q w - - 0 1")
    val game = fenToGame(position, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Some(White))

  test("racingKings should recognize a king in the goal - black"):
    val position = FullFen("6k1/8/8/8/8/2r5/1KB5/2B5 w - - 0 1")
    val game = fenToGame(position, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Some(Black))

  test("racingKings should give black one more move when white is in the goal"):
    val position = FullFen("2K5/5k2/8/8/8/8/8/8 b - - 0 1")
    val game = fenToGame(position, RacingKings)
    assertNot(game.position.end)

  test("racingKings should give black one more move but not if it does not matter anyway"):
    val position = FullFen("2K5/8/2n1nk2/8/8/8/8/4r3 b - - 0 1")
    val game = fenToGame(position, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Some(White))

  test("racingKings should call it a draw with both kings in the goal"):
    val position = FullFen("2K2k2/8/8/8/8/1b6/1b6/8 w - - 0 1")
    val game = fenToGame(position, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.status, Status.Draw.some)

  test("racingKings initialize the board without castling rights"):
    assert(RacingKings.initialPosition.history.castles.isEmpty)

  List(
    "1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
    "1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4",
    "rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4",
    "1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4",
    "2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
    "1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"
  ).foreach: fen =>
    test(s"racingKings validate board correctly $fen"):
      val game = Fen
        .read(RacingKings, FullFen(fen))
        .get
      assertNot(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

  test("antichess initialize the board without castling rights"):
    assert(Antichess.initialPosition.history.castles.isEmpty)

  test("antichess calculate material imbalance"):
    val position = FullFen("8/p7/8/8/2B5/b7/PPPK2PP/RNB3NR w - - 1 16")
    val game = fenToGame(position, Antichess)
    assertEquals(game.position.materialImbalance, -20)

  List(
    "1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
    "1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4",
    "rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4",
    "1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4",
    "2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
    "1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4",
    "r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"
  ).foreach: fen =>
    test(s"antichess validate board correctly $fen"):
      val game = Fen
        .read(Antichess, FullFen(fen))
        .get
      assert(game.variant.valid(game, true))
      assert(game.variant.valid(game, false))

  test("racingKings validate board correctly with any check at all for white"):
    val position = FullFen("8/8/8/k5R1/8/8/1rbnNB1K/qrbnNBRQ b - - 0 1")
    val game = fenToGame(position, RacingKings)
    assertNot(game.position.playable(true))
    assert(game.position.playable(false))

  test("racingKings validate board correctly with any check at all for black"):
    val position = FullFen("8/8/8/k7/7r/8/2bnNBRK/qrbnNBRQ w - - 0 1")
    val game = fenToGame(position, RacingKings)
    assertNot(game.position.playable(true))
    assert(game.position.playable(false))

  test("horde validate board correctly: two-step pawn advance with no check should be valid"):
    val position = FullFen("2r3k1/p2P1pp1/1p5p/3p4/P7/PP6/2P3P1/8 b - - 1 36")
    val game = fenToGame(position, Horde).playMoves(A7 -> A5).get
    assert(game.position.playable(true))

  test(
    "horde validate board correctly: when previous move is a double pawn push and checker is not the pushed pawn or a sliding piece"
  ):
    val game = Fen
      .read(Horde, FullFen("1r6/6q1/8/3k4/2pPP3/8/PPP2PPP/PPPPPPPP b - d3 0 1"))
      .get
    assert(game.variant.valid(game, false))
    assertNot(game.variant.valid(game, true))

  test(
    "horde validate board correctly: when previous move is a double pawn push and the only checker is a rook but not discovered check"
  ):
    val game = Fen
      .read(
        Horde,
        FullFen("5r2/8/4k2R/8/3pP3/8/PPPP1PPP/2PPPPP1 b - e3 0 1")
      )
      .get
    assertNot(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test(
    "horde validate board correctly: when previous move is a double pawn push and the only checker is a bishop but not discovered check"
  ):
    val game = Fen
      .read(Horde, FullFen("5r2/8/4k3/8/3pP1B1/8/PPPP1PPP/2PPPPP1 b - e3 0 1"))
      .get
    assertNot(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test("horde validate board correctly: when multiple checkers are aligned with the king"):
    val game = Fen
      .read(Horde, FullFen("1q6/8/R2k1R2/8/8/8/8/8 b - - 0 1"))
      .get
    assertNot(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test(
    "horde validate board correctly: when previous move is a double pawn push and the only checker is the pushed pawn"
  ):
    val game = Fen
      .read(Horde, FullFen("1r6/6q1/8/4k3/2pP4/2P5/PP3PPP/PPPPPPPP b - d3 0 3"))
      .get
    assert(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test("horde validate board correctly: when two checkers are not on the same rank, file or diagonal"):
    val game = Fen
      .read(Horde, FullFen("7r/3k4/8/1B2N2q/1B6/8/PPPPPPPP/PPPPPPPP w - - 0 1"))
      .get
    assert(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test(
    "horde validate board correctly: when previous move is a double pawn push and the only checker is a discovered rook check"
  ):
    val game = Fen
      .read(Horde, FullFen("8/8/8/8/3Pp3/8/k5R1/8 b - d3 0 2"))
      .get
    assert(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test(
    "horde validate board correctly: when previous move is a double pawn push and the only checker is a discovered bishop check"
  ):
    val game = Fen
      .read(Horde, FullFen("8/8/8/8/1k1Pp3/8/8/4B3 b - d3 0 2"))
      .get
    assert(game.variant.valid(game, true))
    assert(game.variant.valid(game, false))

  test("promotion without promotion role should default to queen"):
    val position = Fen.read(Standard, FullFen("8/P4p2/4p3/4P3/1K3k2/8/1P4p1/8 b - - 0 43")).get
    position.variant
      .move(position, G2, G1, none)
      .assertRight: move =>
        assertEquals(move.promotion, Queen.some)

  test("promotion with promotion role should be the same"):
    val position = Fen.read(Standard, FullFen("8/P4p2/4p3/4P3/1K3k2/8/1P4p1/8 b - - 0 43")).get
    position.variant
      .move(position, G2, G1, Knight.some)
      .assertRight: move =>
        assertEquals(move.promotion, Knight.some)
