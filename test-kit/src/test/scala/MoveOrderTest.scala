package chess

import chess.format.Fen
import chess.perft.Perft
import chess.variant.*
import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

/** Guards the *order* in which move generation emits moves.
  *
  * Perft validates move counts but says nothing about ordering, so a refactor of the
  * generators can keep every perft number green while permuting the move list. lila may
  * depend on that order (UI move lists, `dests` payloads), so it is pinned here.
  */
class MoveOrderTest extends MunitExtensions with SnapshotAssertions:

  private def moveOrder(perfts: List[Perft], variant: Variant): String =
    perfts
      .map: perft =>
        val position = Fen
          .read(variant, perft.epd)
          .getOrElse(throw RuntimeException(s"Invalid fen: ${perft.epd} for variant: $variant"))
        val moves = variant.validMoves(position).map(_.toUci.uci).mkString(" ")
        s"${perft.epd}\n$moves"
      .mkString("\n\n")

  test("move order: random (chess960)"):
    assertFileSnapshot(moveOrder(Perft.randomPerfts, Chess960), "moveorder/random.txt")

  test("move order: tricky"):
    assertFileSnapshot(moveOrder(Perft.trickyPerfts, Standard), "moveorder/tricky.txt")

  test("move order: chess960"):
    assertFileSnapshot(moveOrder(Perft.chess960, Chess960), "moveorder/chess960.txt")

  test("move order: threeCheck"):
    assertFileSnapshot(moveOrder(Perft.threeCheckPerfts, ThreeCheck), "moveorder/threecheck.txt")

  test("move order: antichess"):
    assertFileSnapshot(moveOrder(Perft.antichessPerfts, Antichess), "moveorder/antichess.txt")

  test("move order: atomic"):
    assertFileSnapshot(moveOrder(Perft.atomicPerfts, Atomic), "moveorder/atomic.txt")

  test("move order: crazyhouse"):
    assertFileSnapshot(moveOrder(Perft.crazyhousePerfts, Crazyhouse), "moveorder/crazyhouse.txt")

  test("move order: horde"):
    assertFileSnapshot(moveOrder(Perft.hordePerfts, Horde), "moveorder/horde.txt")

  test("move order: racingKings"):
    assertFileSnapshot(moveOrder(Perft.racingkingsPerfts, RacingKings), "moveorder/racingkings.txt")
