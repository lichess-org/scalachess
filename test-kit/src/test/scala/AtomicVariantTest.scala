package chess

import cats.syntax.all.*
import chess.format.FullFen
import chess.format.pgn.SanStr
import chess.variant.Atomic

class AtomicVariantTest extends ChessTest:

  test("Must explode surrounding non pawn pieces on capture"):
    val fenPosition     = FullFen("rnbqkbnr/1ppppp1p/p5p1/8/8/1P6/PBPPPPPP/RN1QKBNR w KQkq -")
    val game            = fenToGame(fenPosition, Atomic)
    val explodedSquares = List(Square.H8, Square.G8)
    val intactPawns     = List(Square.F7, Square.G6, Square.H7)

    game
      .playMoves((Square.B2, Square.H8))
      .assertRight: game =>
        assert(explodedSquares.forall(square => game.situation.board(square).isEmpty))
        assert(intactPawns.forall(square => game.situation.board(square).isDefined))

  test("Must explode all surrounding non pawn pieces on capture (contrived situation)"):
    val fenPosition = FullFen("k7/3bbn2/3rqn2/3qr3/8/7B/8/1K6 w - -")
    val game        = fenToGame(fenPosition, Atomic)
    val explodedSquares =
      List(Square.D5, Square.E5, Square.D6, Square.E6, Square.F6, Square.D7, Square.E7, Square.F7)

    game
      .playMoves((Square.H3, Square.E6))
      .assertRight: game =>
        assert(explodedSquares.forall(square => game.situation.board(square).isEmpty))

  test(
    "Must explode all surrounding non pawn pieces on capture (contrived situation with bottom right position)"
  ):
    val fenPosition = FullFen("k7/3bbn2/3rqn2/4rq2/8/1B6/8/K7 w - -")
    val game        = fenToGame(fenPosition, Atomic)
    val explodedSquares =
      List(Square.F5, Square.E5, Square.D6, Square.E6, Square.F6, Square.D7, Square.E7, Square.F7)
    game
      .playMoves((Square.B3, Square.E6))
      .assertRight: game =>
        assert(explodedSquares.forall(square => game.situation.board(square).isEmpty))

  test("Not allow a king to capture a piece"):
    val fenPosition = FullFen("8/8/8/1k6/8/8/8/1Kr5 w - -")
    val game        = fenToGame(fenPosition, Atomic)
    assertEquals(game.playMoves((Square.B1, Square.C1)), Left(ErrorStr("Piece on b1 cannot move to c1")))

  test("The game must end with the correct winner when a king explodes in the perimeter of a captured piece"):
    val fenPosition = FullFen("rnb1kbnr/ppp1pppp/8/3q4/8/7P/PPPP1PP1/RNBQKBNR b KQkq -")
    val game        = fenToGame(fenPosition, Atomic)
    game
      .playMoves((Square.D5, Square.D2))
      .assertRight: g =>
        assert(g.situation.end)
        assert(g.situation.variantEnd)
        assertEquals(g.situation.winner, Some(Black))

  test("The game must end by a traditional checkmate (atomic mate)"):
    val fenPosition = FullFen("1k6/8/8/8/8/8/PP5r/K7 b - -")
    val game        = fenToGame(fenPosition, Atomic)
    game
      .playMoves((Square.H2, Square.H1))
      .assertRight: g =>
        assert(g.situation.end)
        assertNot(g.situation.variantEnd)
        assertEquals(g.situation.winner, Some(Black))

  test("Must be a stalemate if a king could usually take a piece, but can't because it would explode"):
    val positionFen = FullFen("k7/8/1R6/8/8/8/8/5K2 w - -")
    val game        = fenToGame(positionFen, Atomic)
    game
      .playMoves((Square.B6, Square.B7))
      .assertRight: g =>
        assert(g.situation.end)
        assert(g.situation.staleMate)

  test("It is stalemate if there are only two kings and two opposite square coloured bishops remaining"):
    val positionFen = FullFen("4K3/8/2b5/8/8/8/5B2/3k4 b - -")
    val game        = fenToGame(positionFen, Atomic)
    assert(game.situation.end)
    assert(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)
    assertEquals(game.situation.status, Some(Status.Draw))

  test(
    "In atomic check, an opportunity at exploding the opponent's king takes priority over getting out of check"
  ):
    val positionFen = FullFen("k1K5/pp5R/8/8/3Q4/P7/1P6/2r5 w - -")
    val game        = fenToGame(positionFen, Atomic)
    assert(game.situation.check.yes)
    assertNot(game.situation.end)
    assertEquals(game.situation.winner, None)
    assertEquals(game.situation.moves.keySet, Set(Square.D4, Square.H7, Square.C8))
    game.situation.moves
      .get(Square.D4)
      .assertSome: moves =>
        // The queen can defend the king from check
        assert(moves.find(_.dest == Square.A7).isDefined)
        // Or explode the opponent's king to win the game
        assert(moves.find(_.dest == Square.C4).isDefined)

    // The king cannot capture a piece in the perimeter of the opponent king, exploding itself
    game.situation.moves
      .get(Square.C8)
      .assertSome: m =>
        assertNot(m.forall(_.captures))

    // The rook cannot capture, as that would result in our own king exploding
    game.situation.moves
      .get(Square.H7)
      .assertSome: mvs =>
        assertEquals(mvs.find(_.captures), None)
        // It can, however, defend the king
        assert(mvs.find(_.dest == Square.C7).isDefined)
        assertEquals(mvs.size, 1)

  test(
    "In atomic mate, an opportunity at exploding the opponent's king takes priority over getting out of mate"
  ):
    val positionFen = FullFen("k1r5/pp5R/8/8/3Q4/8/PP6/K7 b - -")
    val game        = fenToGame(positionFen, Atomic)
    game
      .playMoves((Square.C8, Square.C1))
      .assertRight: game =>
        assertNot(game.situation.end)
        assertEquals(game.situation.winner, None)
        assertEquals(game.situation.moves.keySet, Set(Square.D4, Square.H7))
        assert(game.situation.legalMoves.forall(_.captures))

  test(
    "In atomic chess a king may walk into a square that is in the perimeter of the opponent king since it can't capture"
  ):
    val positionFen = FullFen("3k4/8/3K4/8/8/8/7r/8 w - -")
    val game        = fenToGame(positionFen, Atomic)
    game
      .playMoves((Square.D6, Square.D7))
      .assertRight: game =>
        assert(game.situation.board(Square.D7).isDefined)
        assertNot(game.situation.check.yes)

  test("Draw on knight and king vs king"):
    val position = FullFen("8/1n6/8/8/8/8/k7/2K1b2R w - -")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.H1, Square.E1))
      .assertRight: game =>
        assert(game.situation.end)
        assertEquals(game.situation.status, Some(Status.Draw))

  test("Draw on bishop and king vs king"):
    val position = FullFen("8/1b6/8/8/8/8/k7/2K1n2R w - -")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.H1, Square.E1))
      .assertRight: game =>
        assert(game.situation.end)
        assertEquals(game.situation.status, Some(Status.Draw))

  test("Draw on a rook and king vs king"):
    val position = FullFen("8/8/8/8/8/8/N4r2/5k1K b - -")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.F2, Square.A2))
      .assertRight: game =>
        assert(game.situation.end)
        assertEquals(game.situation.status, Some(Status.Draw))

  test("Draw on a king vs a king"):
    val position = FullFen("6r1/8/8/1k6/8/8/2K5/6R1 w - -")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.G1, Square.G8))
      .assertRight: game =>
        assert(game.situation.end)
        assertEquals(game.situation.status, Some(Status.Draw))

  test("It should not be possible to capture a piece resulting in your own king exploding"):
    val position = FullFen("rnbqkbnr/pppNp1pp/5p2/3p4/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 3")
    val game     = fenToGame(position, Atomic)
    assertEquals(game.playMoves((Square.D8, Square.D7)), Left(ErrorStr("Piece on d8 cannot move to d7")))

  test(
    "In an en-passant capture, the pieces surrounding the pawn's destination are exploded along with the pawn"
  ):
    val position = FullFen("4k3/2pppb1p/3r1r2/3P1b2/8/8/1K6/4NB2 b - -")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.E7, Square.E5), (Square.D5, Square.E6))
      .assertRight: game =>
        assertEquals(game.situation(Square.E6), None)
        // Every piece surrounding the en-passant destination square that is not a pawn should be empty
        import bitboard.Bitboard.*
        assert:
          Square.E6.kingAttacks.forall: square =>
            game.situation(square).isEmpty || square == Square.E7 || square == Square.D7

  test("Verify it is not possible to walk into check"):
    val position = FullFen("rnbqkbnr/ppp1pppp/8/3pN3/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 2")
    val game     = fenToGame(position, Atomic)
    assertEquals(game.playMoves((Square.E8, Square.D7)), Left(ErrorStr("Piece on e8 cannot move to d7")))

  test("Verify that a king can move into what would traditionally be check when touching the opponent king"):
    val position = FullFen("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
    val game     = fenToGame(position, Atomic)
    assert(game.playMoves((Square.F6, Square.F5)).isRight)

  test("After kings have been touching, and one moves away, a king that was protected is under attack again"):
    val position = FullFen("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.F6, Square.F5), (Square.G4, Square.H3))
      .assertRight: game =>
        assertEquals(game.situation.check, Check.Yes)

  test("Can move into discovered check in order to explode the opponent's king"):
    val position = FullFen("R2r2k1/1p2ppbp/8/6p1/2p5/5P1N/P2Pn1PP/2B1K2R b K - 3 19")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.D8, Square.D2))
      .assertRight: game =>
        assert(game.situation.end)
        assertEquals(game.situation.winner, Some(Black))

  test(
    "It must be possible to remove yourself from check by exploding a piece next to the piece threatening the king"
  ):
    val position = FullFen("5k1r/p1ppq1pp/5p2/1B6/1b3P2/2P5/PP4PP/RNB1K2R w KQ - 0 12")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.B5, Square.D7))
      .assertRight: game =>
        assertEquals(game.situation.check, Check.No)

  test(
    "It should not be possible to explode a piece, exploding a piece next to it which would result in a check"
  ):
    val position = FullFen("r1b1k2r/pp1pBppp/2p1p2n/q3P3/B2P4/2N2Q2/PPn2PPP/R3K1NR w KQkq - 9 11")
    val game     = fenToGame(position, Atomic)
    assertEquals(game.playMoves((Square.A4, Square.C2)), Left(ErrorStr("Piece on a4 cannot move to c2")))

  test(
    "Game is not a draw when the last piece a player has other than their king is a pawn that is blocked by a mobile piece"
  ):
    val position = FullFen("3Q4/2b2k2/5P2/8/8/8/6K1/8 b - - 0 57")
    val game     = fenToGame(position, Atomic)
    game
      .playMoves((Square.C7, Square.D8))
      .assertRight: game =>
        assertNot(game.situation.end)

  // This would probably be harmless, but there might be a use case where the count of available moves matters,
  // or similar, so best to code defensively.
  test("There are no repeated moves in the list of available moves for the situation"):
    // Situation where the queen can capture a pawn to both win and remove itself from check
    val position = FullFen("k1r5/pp5Q/8/8/8/8/PP6/2K5 w - -")
    val game     = fenToGame(position, Atomic)
    game.situation.moves
      .get(Square.H7)
      .assertSome: queenMoves =>
        assertEquals(queenMoves.size, queenMoves.toSet.size)

  test("End move regression: from init"):
    import Square.*
    val game = fenToGame(format.Fen.initial, Atomic)
    game
      .playMoves(E2 -> E4, D7 -> D5, G1 -> F3, D5 -> E4, F1 -> B5, D8 -> D2)
      .assertRight: g =>
        assert(g.situation.variantEnd)
  test("End move regression: from position"):
    import Square.*
    val game = fenToGame(FullFen("rnbqkbnr/ppp1pppp/8/1B6/8/8/PPPP1PPP/RNBQK2R b KQkq - 1 1"), Atomic)
    game
      .playMoves(D8 -> D2)
      .assertRight: g =>
        assert(g.situation.variantEnd)

  test("Not escaping a check that would blow up both kings"):
    val position = FullFen("rnbq1bnr/pp1pp1pp/8/2pk1p2/3K1P2/P6P/1PPPP1P1/RNBQ1BNR b - - 0 6")
    assert(
      fenToGame(position, Atomic)
        .playMoves(
          Square.A7 -> Square.A6
        )
        .isRight
    )

  test("Identify that a player does not have sufficient material to win when they only have a king"):
    val position = FullFen("8/8/8/8/7p/2k4q/2K3P1/8 w - - 19 54")
    val game     = fenToGame(position, Atomic)
    assertNot(game.situation.end)
    game
      .playMoves(Square.G2 -> Square.H3)
      .assertRight: game =>
        assert(game.situation.opponentHasInsufficientMaterial)

  test("An automatic draw in a closed position with only kings and pawns which cannot move"):
    val position = FullFen("8/8/6p1/3K4/6P1/2k5/8/8 w - -")
    fenToGame(position, Atomic)
      .playMoves(Square.G4 -> Square.G5)
      .assertRight: game =>
        assert(game.situation.autoDraw)
        assert(game.situation.end)

  test("Not draw inappropriately on bishops vs bishops (where an explosion taking out the king is possible)"):
    val position = FullFen("B2BBBB1/7P/8/8/8/8/3kb3/4K3 w - - 1 53")
    fenToGame(position, Atomic)
      .playMove(Square.H7, Square.H8, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on two bishops (of both square colors)"):
    val position = FullFen("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on bishop and knight"):
    val position = FullFen("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Knight.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on three bishops (of both square colors)"):
    val position = FullFen("8/5k2/8/8/8/8/4pKB1/5B2 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on three bishops (of both square colors)"):
    val position = FullFen("8/5k2/8/8/8/8/4pKB1/6B1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on two bishops and a knight"):
    val position = FullFen("8/5k2/8/8/8/8/4pKB1/6N1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on two bishops and a knight"):
    val position = FullFen("8/5k2/8/8/8/8/4pKN1/6B1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on two knights and a bishop"):
    val position = FullFen("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Bishop.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on three knights (of two colors)"):
    val position = FullFen("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Knight.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on three knights (of two colors)"):
    val position = FullFen("8/5k2/8/8/8/8/4pKN1/6n1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Knight.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("Not draw inappropriately on three knights (of the same color)"):
    val position = FullFen("8/5k2/8/8/8/8/4pKn1/6n1 b - - 1 44")
    fenToGame(position, Atomic)
      .playMove(Square.E2, Square.E1, Knight.some)
      .assertRight: g =>
        assertNot(g.situation.end)

  test("An automatic draw in a closed position with kings, pawns and a pawnitized bishop"):
    val position = FullFen("8/8/2k1p3/5p2/4PP2/1b6/4K3/8 w - - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.E4, Square.E5)
      .assertRight: game =>
        assert(game.situation.end)
        assert(game.situation.autoDraw)

  test("Not draw inappropriately on blocked pawns with a non-pawnitized bishop"):
    val position = FullFen("8/8/2k5/5p2/8/2b2P2/8/3K4 w - - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.F3, Square.F4)
      .assertRight: game =>
        assertNot(game.situation.autoDraw)
        assertNot(game.situation.end)

  test("Not draw inappropriately if both sides have a pawnitized bishop"):
    val position = FullFen("6bk/4B2p/8/7P/4K3/8/8/8 w - - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.H5, Square.H6)
      .assertRight: game =>
        assertNot(game.situation.autoDraw)
        assertNot(game.situation.end)

  test("Checkmate overrides closed position"):
    val position = FullFen("8/8/b1p5/kpP5/p3K3/PP6/8/8 w - - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.B3, Square.B4)
      .assertRight: game =>
        assertNot(game.situation.autoDraw)
        assert(game.situation.end)

  test("Replay an entire game"):
    val sans: Vector[SanStr] =
      SanStr.from(
        "Nf3 f6 e3 d5 Ng5 fxg5 Qh5+ g6 Qe5 Be6 Bb5+ c6 Qc7 Qxc7 b3 d4 Nc3 dxc3 Bc4 O-O-O O-O h5 Ba3 c5 Bd5 b5 Bb7+ Kb8 c4 h4 d4 Nf6 h3 Ng4 hxg4 h3 g4 h2+ Kh1 Bg7 Rad1 b4 Bb2 Bf5 f3 Bd3 Rxd3 Rh3 d5 Rg3 Be5+ Bxe5 d6 Rg1#"
          .split(' ')
          .toVector
      )
    val (game, steps, error) = chess.Replay.gameMoveWhileValid(sans, Atomic.initialFen, Atomic)
    assertEquals(error, None)
    assertEquals(steps.size, sans.size)

  test("Allow castling with touching kings and rook shielding final attack"):
    val position = FullFen("8/8/8/8/8/8/4k3/R3K2r w Q - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.E1, Square.C1)
      .assertRight: game =>
        assertEquals(game.situation(Square.C1), White.king.some)
        assertEquals(game.situation(Square.D1), White.rook.some)

  test("Allow castling with touching kings and rook shielding final attack 2"):
    val position = FullFen("r3k1rR/5K2/8/8/8/8/8/8 b kq - 0 1")
    fenToGame(position, Atomic)
      .playMoves((Square.G8, Square.G6), (Square.F7, Square.E7), (Square.E8, Square.A8))
      .assertRight: game =>
        assertEquals(game.situation(Square.C8), Black.king.some)
        assertEquals(game.situation(Square.D8), Black.rook.some)

  test("Disallow castling through atomic check"):
    val position = FullFen("8/8/8/8/8/8/5k2/R3K2r w Q - 0 1")
    assert(fenToGame(position, Atomic).playMove(Square.E1, Square.C1).isLeft)

  test("Disallow castling into atomic check"):
    val position = FullFen("4k3/8/8/8/8/8/8/rR2K3 w Q - 0 1")
    assert(fenToGame(position, Atomic).playMove(Square.E1, Square.B1).isLeft)

  test("Exploded rooks can't castle"):
    val position = FullFen("1r2k3/8/8/8/8/8/1P6/1R2K3 b Q - 0 1")
    fenToGame(position, Atomic)
      .playMove(Square.B8, Square.B2)
      .assertRight: game =>
        assert(game.situation.legalMoves.filter(_.castles).isEmpty)

  test("Unmoved rooks correctly updated after explosion, lila issue-14544"):
    val sans: Vector[SanStr] =
      SanStr.from(
        "e4 d5 d4 e6 Nc3 b5 Bg5 f6 Bh6 Ba3 Bxg7 h5 bxa3 c5 Qc1 Qe7 Qh6 Qg7 Qh8+ Qxh8 Rb1 cxd4 Bxb5 Nd7 Rb7 Kf8 Rxd7 Rb8 Ne2 Rb1+ Nc1 d4 O-O"
          .split(' ')
          .toVector
      )
    val (game, steps, error) = chess.Replay.gameMoveWhileValid(sans, Atomic.initialFen, Atomic)
    assertEquals(error, None)
    assertEquals(steps.size, sans.size)
