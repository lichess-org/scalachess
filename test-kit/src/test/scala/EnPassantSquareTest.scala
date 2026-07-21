package chess

import chess.format.Fen
import chess.perft.Perft
import chess.variant.*

// Position.enPassantSquare is computed from the en passant candidates alone
// (Variant.isLegalEnPassant) instead of generating every legal move; this
// verifies it agrees with the full-movegen derivation on the perft suites.
class EnPassantSquareTest extends munit.FunSuite:

  val fixtures: List[(String, List[Perft], Variant)] = List(
    ("threeCheck", Perft.threeCheckPerfts, ThreeCheck),
    ("antichess", Perft.antichessPerfts, Antichess),
    ("atomic", Perft.atomicPerfts, Atomic),
    ("crazyhouse", Perft.crazyhousePerfts, Crazyhouse),
    ("horde", Perft.hordePerfts, Horde),
    ("racingkings", Perft.racingkingsPerfts, RacingKings),
    ("tricky", Perft.trickyPerfts, Chess960),
    ("tricky as standard", Perft.trickyPerfts, Standard),
    ("chess960", Perft.chess960, Chess960)
  )

  val depth = 3
  val nodesPerRoot = 1_000

  fixtures.foreach: (name, perfts, variant) =>
    test(s"enPassantSquare == legalMoves.find(_.enpassant).map(_.dest): $name"):
      perfts.foreach: perft =>
        Fen
          .read(variant, perft.epd)
          .foreach(position => check(position, depth, nodesPerRoot): Unit)

  private def check(position: Position, depth: Int, budget: Int): Int =
    assertEquals(
      position.enPassantSquare,
      position.legalMoves.find(_.enpassant).map(_.dest),
      s"fen: ${Fen.write(position)}"
    )
    var remaining = budget - 1
    if depth > 0 then
      position.legalMoves.foreach: move =>
        if remaining > 0 then remaining = check(move.after, depth - 1, remaining)
    remaining
