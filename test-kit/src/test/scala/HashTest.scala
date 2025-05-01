package chess

import chess.format.{ Fen, FullFen, Uci }

import Square.*
import variant.{ Antichess, Atomic, Crazyhouse, Standard, ThreeCheck }

class HashTest extends ChessTest:

  test("Polyglot hasher: match on the starting position"):
    val fen  = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x463b_9618))

  test("Polyglot hasher: match after 1. e4"):
    val fen  = FullFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x823c_9b50))

  test("Polyglot hasher: match after 1. e4 d5"):
    val fen  = FullFen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x0756_b944))

  test("Polyglot hasher: match after 1. e4 d5 2. e5"):
    val fen  = FullFen("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x662f_afb9))

  test("Polyglot hasher: match after 1. e4 d5 2. e5 f5"):
    // note that en-passant matters
    val fen  = FullFen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x22a4_8b5a))

  test("Polyglot hasher: match after 1. e4 d5 2. e5 f5 3. Ke2"):
    // 3. Ke2 forfeits castling rights
    val fen  = FullFen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 3")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x652a_607c))

  test("Polyglot hasher: match after 1. e4 d5 2. e5 f5 3. Ke2 Kf7"):
    val fen  = FullFen("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 2 4")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x00fd_d303))

  test("Polyglot hasher: match after 1. a4 b5 2. h4 b4 3. c4"):
    // again, note en-passant matters
    val fen  = FullFen("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x3c81_23ea))

  test("Polyglot hasher: match after 1. a4 b5 2. h4 b4 3. c4 bxc3 4. Ra3"):
    // 4. Ra3 partially forfeits castling rights
    val fen  = FullFen("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 1 4")
    val game = fenToGame(fen, Standard)
    assertEquals(Hash(game.board), Hash(0x5c3f_9b82))

  // Variants

  test("Hasher: account for checks in three-check"):
    // 2 ... Bb4+
    val gameA = Game(Position.init(ThreeCheck, White))
      .playMoves(
        E2 -> E4,
        E7 -> E6,
        D2 -> D4,
        F8 -> B4
      )
      .toOption
      .get

    // repeat
    val gameB = gameA
      .playMoves(
        C1 -> D2,
        B4 -> F8,
        D2 -> C1,
        F8 -> B4
      )
      .toOption
      .get

    assertNotEquals(Hash(gameA.board), Hash(gameB.board))

  test("Hasher: account for pockets in crazyhouse"):
    val gameA = Game(Crazyhouse)
      .playMoves(E2 -> E4, D7 -> D5, E4 -> D5)
      .get

    val intermediate = Game(Crazyhouse)
      .playMoves(E2 -> E4, D7 -> D5, E4 -> D5, D8 -> D7)
      .get

    // we reach the same position, but now the pawn is in blacks pocket
    val gameB = intermediate(Uci.Drop(Pawn, D6)).get._1
      .playMoves(
        D7 -> D6,
        D1 -> E2,
        D6 -> D8,
        E2 -> D1
      )
      .get

    assertNotEquals(Hash(gameA.board), Hash(gameB.board))

  test("Hasher: be consistent in crazyhouse"):
    // from https://lichess.org/j4r7XHTB/black
    val fen           = FullFen("r2qkb1r/ppp1pppp/2n2n2/3p2B1/3P2b1/4PN2/PPP1BPPP/RN1QK2R/ b KQkq - 9 5")
    val board         = Fen.read(Crazyhouse, fen).get
    val move          = board.move(Square.G4, Square.F3, None).get
    val hashAfterMove = Hash(move.boardAfter)

    // 5 ... Bxf3
    val fenAfter   = FullFen("r2qkb1r/ppp1pppp/2n2n2/3p2B1/3P4/4Pb2/PPP1BPPP/RN1QK2R/n w KQkq - 10 6")
    val boardAfter = Fen.read(Crazyhouse, fenAfter).get
    val hashAfter  = Hash(boardAfter)

    assertEquals(hashAfterMove, hashAfter)

  test("Hasher: be consistent when king is captured in antichess"):
    val fen           = FullFen("rnbqkb1r/ppp1pppp/3p1n2/1B6/8/4P3/PPPP1PPP/RNBQK1NR w KQkq - 2 3")
    val board         = Fen.read(Antichess, fen).get
    val move          = board.move(Square.B5, Square.E8, None).get
    val hashAfterMove = Hash(move.boardAfter)

    // 3. BxK
    val fenAfter   = FullFen("rnbqBb1r/ppp1pppp/3p1n2/8/8/4P3/PPPP1PPP/RNBQK1NR b KQkq - 0 3")
    val boardAfter = Fen.read(Antichess, fenAfter).get
    val hashAfter  = Hash(boardAfter)

    assertEquals(hashAfterMove, hashAfter)

  test("Hasher: be consistent when rook is exploded in atomic"):
    val fen           = FullFen("rnbqkb1r/ppppp1pp/5p1n/6N1/8/8/PPPPPPPP/RNBQKB1R w KQkq - 2 3")
    val board         = Fen.read(Atomic, fen).get
    val move          = board.move(Square.G5, Square.H7, None).get
    val hashAfterMove = Hash(move.boardAfter)

    // 3. Nxh7
    val fenAfter   = FullFen("rnbqkb2/ppppp1p1/5p2/8/8/8/PPPPPPPP/RNBQKB1R b KQkq - 0 3")
    val boardAfter = Fen.read(Atomic, fenAfter).get
    val hashAfter  = Hash(boardAfter)

    assertEquals(hashAfterMove, hashAfter)

  test("Index out of bounds when hashing pockets"):
    val fenPosition = FullFen("2q1k1nr/B3bbrb/8/8/8/8/3qN1RB/1Q2KB1R/RRRQQQQQQrrrqqq w Kk - 0 11")
    val game        = fenToGame(fenPosition, Crazyhouse)
    assert(game.apply(E1, D2).isRight)
