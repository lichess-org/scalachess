package chess

import cats.syntax.option.*
import chess.Square.*
import chess.format.FullFen
import chess.format.pgn.SanStr
import chess.variant.Crazyhouse

class CrazyhouseVariantTest extends ChessTest:

  test("nothing to drop"):
    val fenPosition = FullFen("3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25")
    val game = fenToGame(fenPosition, Crazyhouse).updateBoard: b =>
      b.withCrazyData(Crazyhouse.Data.init)
    assert(game.board.checkMate)
    assertNot(game.board.opponentHasInsufficientMaterial)

  test("checkmate"):
    val fenPosition = FullFen("r2q1b1r/ppp1kPpp/2p5/2PpN3/1n1Pb3/3PK3/PPr1BPPP/n1q1N2R/b w - - 0 20")
    val game        = fenToGame(fenPosition, Crazyhouse)
    assert(game.board.checkMate)

  test("pieces to drop, in vain"):
    val fenPosition = FullFen("3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25")
    val game = fenToGame(fenPosition, Crazyhouse).updateBoard: b =>
      b.withCrazyData(
        Crazyhouse.Data(
          pockets = ByColor(
            Crazyhouse.Pocket(Queen :: Nil),
            Crazyhouse.Pocket(Rook :: Pawn :: Pawn :: Nil)
          ),
          promoted = Bitboard.empty
        )
      )
    assert(game.board.checkMate)
    assertNot(game.board.opponentHasInsufficientMaterial)

  test("autodraw: tons of pointless moves but shouldn't apply 50-moves"):
    val moves = List.fill(30)(List(B1 -> C3, B8 -> C6, C3 -> B1, C6 -> B8))
    Game(Crazyhouse)
      .playMoves(moves.flatten*)
      .assertRight: g =>
        assertNot(g.variant.fiftyMoves(g.history))
        assert(g.board.autoDraw)
  test("autodraw: from prod should 3fold"):
    val moves = List(
      E2 -> E4,
      E7 -> E6,
      F2 -> F4,
      C7 -> C5,
      E4 -> E5,
      D7 -> D6,
      G1 -> F3,
      B8 -> C6,
      F1 -> B5,
      C8 -> D7,
      B5 -> C6,
      D7 -> C6,
      D2 -> D3,
      C6 -> F3,
      D1 -> F3,
      D6 -> D5,
      E1 -> H1,
      D8 -> B6,
      C1 -> E3,
      B6 -> B2,
      B1 -> D2,
      B2 -> B6,
      A1 -> B1,
      B6 -> C7,
      C2 -> C4,
      A8 -> D8,
      C4 -> D5,
      D8 -> D5,
      D2 -> E4,
      B7 -> B6,
      F1 -> D1,
      G8 -> H6,
      G2 -> G4,
      F8 -> E7,
      G1 -> G2,
      E8 -> H8,
      H2 -> H3,
      F8 -> D8,
      B1 -> B3,
      C5 -> C4,
      B3 -> C3,
      E7 -> B4,
      C3 -> C1,
      D5 -> D3,
      D1 -> D3,
      D8 -> D3,
      C1 -> C2,
      C4 -> C3,
      H3 -> H4,
      C7 -> C6,
      E4 -> F6,
      G7 -> F6,
      F3 -> C6,
      D3 -> E3,
      E5 -> F6,
      H6 -> G4,
      C6 -> C8,
      B4 -> F8,
      C2 -> C3,
      E3 -> E4,
      G2 -> F3,
      G4 -> F6,
      C8 -> D8,
      G8 -> G7,
      D8 -> F6,
      G7 -> F6,
      F3 -> E4,
      F8 -> C5,
      A2 -> A4,
      F6 -> G6,
      A4 -> A5,
      G6 -> H5,
      A5 -> B6,
      A7 -> B6,
      C3 -> G3,
      H5 -> H4,
      G3 -> G7,
      H7 -> H5,
      G7 -> F7,
      H4 -> G3,
      F7 -> F6,
      H5 -> H4,
      F6 -> E6,
      H4 -> H3,
      E6 -> G6,
      G3 -> F2,
      G6 -> H6,
      F2 -> G2,
      H6 -> G6,
      G2 -> F2,
      G6 -> H6,
      F2 -> G2,
      H6 -> G6,
      G2 -> F2,
      G6 -> H6,
      F2 -> G2,
      H6 -> G6
    )
    Game(Crazyhouse)
      .playMoves(moves*)
      .assertRight: g =>
        assert(g.history.threefoldRepetition)
  test("autodraw: from prod with captures and drops should 3fold"):
    chess.Replay
      .boards(
        sans = SanStr.from(
          "e4 e5 Nf3 Nc6 Bc4 Bc5 d3 d6 Nc3 h6 O-O Nf6 Be3 Bg4 Na4 b6 Nxc5 bxc5 B@b7 Nd4 Bxd4 cxd4 N@c6 O-O Nxd8 Raxd8 Bbd5 B@h5 Bxf7+ Bxf7 P@e7 Bxc4 exf8=Q+ Rxf8 dxc4 B@f7 B@d5 B@h5 Bxf7+ Bxf7 B@f5 B@h5 Bxg4 Bxg4"
            .split(' ')
            .toVector
        ),
        initialFen = None,
        variant = Crazyhouse
      )
      .assertRight: boards =>
        assert(boards.last.history.threefoldRepetition)
  test("autodraw: from prod should not 3fold"):
    val moves = List(
      E2 -> E4,
      E7 -> E6,
      F2 -> F4,
      C7 -> C5,
      E4 -> E5,
      D7 -> D6,
      G1 -> F3,
      B8 -> C6,
      F1 -> B5,
      C8 -> D7,
      B5 -> C6,
      D7 -> C6,
      D2 -> D3,
      C6 -> F3,
      D1 -> F3,
      D6 -> D5,
      E1 -> H1,
      D8 -> B6,
      C1 -> E3,
      B6 -> B2,
      B1 -> D2,
      B2 -> B6,
      A1 -> B1,
      B6 -> C7,
      C2 -> C4,
      A8 -> D8,
      C4 -> D5,
      D8 -> D5,
      D2 -> E4,
      B7 -> B6,
      F1 -> D1,
      G8 -> H6,
      G2 -> G4,
      F8 -> E7,
      G1 -> G2,
      E8 -> H8,
      H2 -> H3,
      F8 -> D8,
      B1 -> B3,
      C5 -> C4,
      B3 -> C3,
      E7 -> B4,
      C3 -> C1,
      D5 -> D3,
      D1 -> D3,
      D8 -> D3,
      C1 -> C2,
      C4 -> C3,
      H3 -> H4,
      C7 -> C6,
      E4 -> F6,
      G7 -> F6,
      F3 -> C6,
      D3 -> E3,
      E5 -> F6,
      H6 -> G4,
      C6 -> C8,
      B4 -> F8,
      C2 -> C3,
      E3 -> E4,
      G2 -> F3,
      G4 -> F6,
      C8 -> D8,
      G8 -> G7,
      D8 -> F6,
      G7 -> F6,
      F3 -> E4,
      F8 -> C5,
      A2 -> A4,
      F6 -> G6,
      A4 -> A5,
      G6 -> H5,
      A5 -> B6,
      A7 -> B6,
      C3 -> G3,
      H5 -> H4,
      G3 -> G7,
      H7 -> H5,
      G7 -> F7,
      H4 -> G3,
      F7 -> F6,
      H5 -> H4,
      F6 -> E6,
      H4 -> H3,
      E6 -> G6,
      G3 -> F2,
      G6 -> H6,
      F2 -> G2,
      H6 -> G6,
      G2 -> F2,
      G6 -> H6
    )
    Game(Crazyhouse)
      .playMoves(moves*)
      .assertRight: g =>
        assertNot(g.history.threefoldRepetition)

  test("autodraw: not draw when only kings left"):
    val fenPosition = FullFen("k6K/8/8/8/8/8/8/8 w - - 0 25")
    val game        = fenToGame(fenPosition, Crazyhouse)
    assertNot(game.board.autoDraw)
    assertNot(game.board.end)
    assertNot(game.board.opponentHasInsufficientMaterial)

  test("destinations prod bug on game VVXRgsQT"):
    assertEquals(
      chess
        .Game(
          Crazyhouse.some,
          FullFen("r2q1b1r/p2k1Ppp/2p2p2/4p3/P2nP2n/3P1PRP/1PPB1K1q~/RN1Q1B2/Npb w - - 40 21").some
        )
        .board
        .destinations
        .toMap,
      Map(
        F2 -> Bitboard(Set(E3, E1)),
        G3 -> Bitboard(Set(G2)),
        F1 -> Bitboard(Set(G2))
      )
    )

  test("replay ZH"):
    assert(
      chess.Replay
        .boards(
          sans = SanStr.from(
            "e4 c5 Na3 d6 Nf3 Bg4 Bc4 Bxf3 Qxf3 N@b4 Bxf7+ Kd7 P@d5 Nf6 O-O Nxc2 Nb5 P@c4 Be6+ Ke8 B@f7#"
              .split(' ')
              .toVector
          ),
          initialFen = None,
          variant = Crazyhouse
        )
        .isRight
    )

  val dropTestCases: List[DropTestCase] = List(
    // Queen check in diagonal
    DropTestCase(
      FullFen("rnb1kbnr/p2p1ppp/1p6/2P1p3/6Pq/5P2/PPP1P2P/RNBQKBNR/P w KQkq - 1 5"),
      Some(Set(F2, G3))
    ),
    // Queen check in column
    DropTestCase(
      FullFen("rnb1kbnr/1pppppPp/p3q3/8/8/8/PPPP1PPP/RNBQKBNR/P w KQkq - 1 3"),
      Some(Set(E2, E3, E4, E5))
    ),
    // bishop check
    DropTestCase(
      FullFen("b2nkbnQ~/p1pppp1p/pP1q2p1/r7/8/R5PR/P1PP1P1P/1NBQ1BNK/R w - - 1 2"),
      Some(Set(G2, F3, E4, D5, C6, B7))
    ),
    // No check
    DropTestCase(FullFen("b3kbnQ~/pnpppp1p/pP1q2p1/3r4/8/R5PR/P1PP1P1P/1NBQ1BNK/R w - - 1 2"), None),
    // No check
    DropTestCase(
      FullFen("b3kb2/pn1ppp1Q~/pPqr2p1/2p5/7R/R5P1/P1PP1PBP/1NBQ2NK/PNR w - - 4 6"),
      None
    ),
    // Double check
    DropTestCase(
      FullFen("b3kbnQ~/pnpppp1p/pP4p1/7q/4r3/R5PR/P1PPKP1P/1NBQ1BN1/R w - - 1 2"),
      Some(Set())
    ),
    // Double check
    DropTestCase(
      FullFen("b3kb1N~/pnpppp1p/pP3rp1/7q/8/R5PR/P1PPKP1P/1NBQ1Bn1/Rn w - - 0 2"),
      Some(Set())
    ),
    // Knight check
    DropTestCase(
      FullFen("b3kb2/pnpppN~1p/pP3rpq/8/3n4/R5PR/P1PPKP1P/1NBQ1BN1/PR w - - 1 3"),
      Some(Set())
    )
  )

  test("possible drops"):
    dropTestCases.foreach:
      case DropTestCase(fen, drops) =>
        val game = fenToGame(fen, Crazyhouse)
        assertEquals(Crazyhouse.possibleDrops(game.board).map(_.toSet), drops)

  test("Index out of bounds when hashing pockets"):
    val fenPosition = FullFen("2q1k1nr/B3bbrb/8/8/8/8/3qN1RB/1Q2KB1R/RRRQQQQQQrrrqqq w Kk - 0 11")
    val game        = fenToGame(fenPosition, Crazyhouse)
    assert(game.apply(E1, D2).isRight)

case class DropTestCase(fen: FullFen, drops: Option[Set[Square]])
