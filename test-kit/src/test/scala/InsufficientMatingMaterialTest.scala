package chess

import chess.format.FullFen
import chess.variant.Standard

import InsufficientMatingMaterial.*
import FortressesCsv.*
import chess.variant.*

class InsufficientMatingMaterialTest extends ChessTest:

  case class KingPathTestCase(
      fen: String,
      color: Color,
      kingShouldBreakthrough: Boolean,
      reachableSquaresForKing: Bitboard
  ):
    val fullFen: FullFen = FullFen(fen)
    val position: Position = fenToGame(fullFen, Standard).position
    val board: Board = position.board
    val opponentColor: Color = !color
    val kingStartPos: Square = board.kingPosOf(color).get
    val attackedByEnemyPawns: Bitboard = board.squaresAttackedByPawns(opponentColor)
    val occupiedByFriendlyPawns: Bitboard = board.byPiece(color, Pawn)
    val occupiedByEnemyPawns: Bitboard = board.byPiece(opponentColor, Pawn)
    val forbiddenSquares: Bitboard = attackedByEnemyPawns | occupiedByFriendlyPawns
    val occupiedByBaseEnemyPawns = occupiedByEnemyPawns & ~attackedByEnemyPawns

  test("bishops on Opposite colors"):

    val trues = List(
      "8/4b3/8/8/8/8/4B3/8 w - - 0 1",
      "8/4b3/8/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "5b2/1k2b1n1/3q4/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "6b1/1k3bn1/3q4/8/2Q5/1K6/4bB2/8 w - - 0 1",
      "6b1/1k3bn1/3B4/8/2Q2B2/1K6/4bB2/8 w - - 0 1",
      "2k2b2/5b2/8/8/8/3R4/1K2Q3/5B2 w - - 0 1",
      "2k2b2/6b1/7b/8/8/3R2B1/1K2Q3/5B2 w - - 0 1",
      "2k5/8/8/8/8/3R2B1/1K2Q3/5B2 w - - 0 1"
    ).map(FullFen(_))

    val falses = List(
      "4b3/8/8/8/8/8/4B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2QB2/8 w - - 0 1",
      "8/8/8/8/8/3R4/1K2B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2Q3/8 w - - 0 1"
    ).map(FullFen(_))

    trues.foreach: fen =>
      assert(bishopsOnOppositeColors(fenToGame(fen, Standard).position.board))

    falses.foreach: fen =>
      assertNot(bishopsOnOppositeColors(fenToGame(fen, Standard).position.board))

  // Determines whether a color does not have mating material.
  test("apply with board and color"):
    val trues = List(
      "8/6R1/K7/2NNN3/5NN1/4KN2/8/k7 w - - 0 1",
      "8/8/K7/8/1k6/8/8/8 w - - 0 1",
      "7k/8/8/8/3K4/8/8/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/8/8 w - - 0 1",
      "krq5/bqqq4/qqr5/1qq5/8/8/8/3qB2K b - -",
      "8/3k4/2q5/8/8/K1N5/8/8 b - -",
      "7k/8/6Q1/8/3K4/8/1n6/8 w - - 0 1"
    ).map(FullFen(_))

    val falses = List(
      "krq5/bqqq4/qqrp4/1qq5/8/8/8/3qB2K b - - 0 1",
      "8/7B/K7/2b5/1k6/8/8/8 b - -",
      "8/8/K7/2b5/1k6/5N2/8/8 b - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/1p6/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/2n5/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/1r6/8/8 w - - 0 1",
      "7k/8/8/5R2/3K4/8/1n6/8 w - - 0 1",
      "7k/8/6B1/8/3K4/8/1n6/8 w - - 0 1",
      "7k/5P2/8/8/3K4/8/1n6/8 w - - 0 1",
      "7k/6N1/8/8/3K4/8/1n6/8 w - - 0 1"
    ).map(FullFen(_))

    trues.foreach: fen =>
      val position = fenToGame(fen, Standard).position
      assert(apply(position, !position.color))

    falses.foreach: fen =>
      val position = fenToGame(fen, Standard).position
      assertNot(apply(position, !position.color))

  test("king paths"):
    val parsed: List[ParsedCase] =
      load("king_pawn_fortresses.csv") match
        case Left(err) => fail("CSV parse error: " + err)
        case Right(cs) => cs

    val testCases: List[KingPathTestCase] =
      parsed.map: pc =>
        val tc = KingPathTestCase(
          fen = pc.fen,
          color = pc.color,
          kingShouldBreakthrough = pc.shouldBreakthrough,
          reachableSquaresForKing = Bitboard.fromKeys(pc.reachables*)
        )
        if tc.reachableSquaresForKing.intersects(tc.forbiddenSquares.add(tc.kingStartPos)) then
          throw new IllegalArgumentException(
            s"reachables may not include kingStartPos or forbidden squares"
          )
        tc

    testCases.foreach: testCase =>
      val illegalSquaresToCheck = testCase.forbiddenSquares.add(testCase.kingStartPos)
      assertEquals(
        kingPathExists(testCase.kingStartPos, testCase.occupiedByBaseEnemyPawns, testCase.forbiddenSquares),
        testCase.kingShouldBreakthrough
      )

      Bitboard.all.squares.foreach: sq =>
        lazy val pathExists =
          kingPathExists(testCase.kingStartPos, Bitboard.apply(sq), testCase.forbiddenSquares)
        if illegalSquaresToCheck.contains(sq) then intercept[IllegalArgumentException](pathExists)
        else assertEquals(pathExists, testCase.reachableSquaresForKing.contains(sq))

  test("detect all pawns locked"):
    val fens: List[String] = List(
      "8/8/8/8/8/8/8/8 w - - 0 1",
      "4k3/8/8/8/8/8/8/4K3 w - - 0 1",
      "4k3/8/8/8/8/4p3/4P3/4K3 w - - 0 1",
      "4k3/8/8/8/8/2p1p3/2P1P3/4K3 w - - 0 1",
      "4k3/8/8/p7/Pp6/1Pp1p3/2P1P3/4K3 w - - 0 1",
      "4k3/p4p2/p4p2/p4p2/Pp2pPp1/1Pp1P1Pp/2P1P2P/4K3 w - - 0 1",
      "4k3/p4p2/p4p2/p4p2/Pp2pPp1/1Pp1P1Pp/2P1P2P/4K3 b - f3 0 1"
    )
    fens.foreach: fen =>
      assert(allPawnsLocked(fenToGame(FullFen(fen), Standard).position.board))

  test("detect not all pawns locked"):
    val fens: List[String] = List(
      "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1",
      "4k3/4p3/8/8/8/8/8/4K3 w - - 0 1",
      "4k3/8/8/8/7p/8/7P/4K3 w - - 0 1",
      "4k3/8/8/p4p2/Pp2pPp1/1Pp3Pp/2P1P2P/4K3 w - - 0 1",
      "4k3/p4p2/p4p2/p4p2/Pp2pPp1/1Pp1P1Pp/2P1P1PP/4K3 b - - 0 1"
    )
    fens.foreach: fen =>
      val position = fenToGame(FullFen(fen), Standard).position
      assertNot(allPawnsLocked(position.board))
      assertNot(kingPawnFortress(position))

  test("detect king pawn fortresses"):
    val fens: List[String] = List(
      "8/2k5/4p2p/3pP2P/1p1Pp1K1/1P2P3/8/8 w - - 0 48",
      "8/2k5/4p2p/3pP2P/1p1Pp1K1/1P2P3/8/8 w - f6 0 48",
      "8/8/2k3p1/1p1p1pPp/1P1P1P1P/5K2/8/8 b - - 80 115",
      "8/8/2k3p1/p2p1pPp/P2P1P1P/5K2/8/8 b - - 0 100",
      "8/8/3k1p1p/2p1pPpP/1pP1P1P1/1P2K3/8/8 b - - 0 60",
      "8/8/1p1k4/1Pp1p1p1/2P1p1P1/1K2P3/8/8 b - - 0 113",
      "8/8/3k4/1p2p3/pP2Pp2/P4Pp1/K5P1/8 w - - 0 58"
    )
    for
      fen <- fens
      variant <- List(Standard, ThreeCheck)
    do
      val position = fenToGame(FullFen(fen), variant).position
      assert(kingPawnFortress(position))
      assert(apply(position))
      assert(apply(position, White))
      assert(apply(position, Black))

  test("detect not king pawn fortresses"):
    val fens: List[String] = List(
      "8/2k5/4p2p/3pP2P/1p1Pp1K1/1P2P3/8/8 w - d6 0 48",
      "8/2k3p1/4p2p/3pP1PP/1p1Pp1K1/1P2P3/8/8 w - - 3 47",
      "8/8/2k3p1/1pPp1pPp/1P1P1P1P/5K2/8/8 b - - 80 115",
      "8/8/2k3p1/1p1p1pPp/1P1P1P1P/2B2K2/8/8 b - - 0 100",
      "8/8/2k3p1/3p1pPp/p2P1P1P/P4K2/8/8 b - - 0 100",
      "8/8/3k1p1p/2p1pPpP/1pP1P1P1/1P1BK3/8/8 b - - 0 60",
      "8/8/1p1k4/1Pp1p1p1/2P1p1P1/1p2P3/1K6/8 w - - 4 113",
      "8/8/3k4/1p2p3/pP2Pp2/5Pp1/PK4P1/8 w - - 0 58",
      "8/8/3k4/1p2p3/pP2Pp2/PK3Pp1/6P1/8 w - - 0 58",
      "8/8/8/1p2pk2/pP2Pp2/P4Pp1/K5P1/8 b - - 0 58"
    )
    for
      fen <- fens
      variant <- List(Standard, ThreeCheck)
    do
      val position = fenToGame(FullFen(fen), variant).position
      assertNot(kingPawnFortress(position))
      assertNot(apply(position))
      assertNot(apply(position, White))
      assertNot(apply(position, Black))
