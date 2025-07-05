package chess
package perft

import cats.effect.IO
import cats.kernel.Monoid
import cats.syntax.all.*
import chess.variant.*
import weaver.*

object PerftTest extends SimpleIOSuite:

  given Monoid[Boolean] with
    def empty                           = true
    def combine(x: Boolean, y: Boolean) = x && y

  val nodeLimits = 1_000L

  test("random.perft"):
    perfts(Perft.randomPerfts, Chess960, 10_000L)
      .map(expect(_))

  test("threeCheck.perft"):
    perfts(Perft.threeCheckPerfts, ThreeCheck, nodeLimits)
      .map(expect(_))

  test("antichess.perft"):
    perfts(Perft.antichessPerfts, Antichess, nodeLimits)
      .map(expect(_))

  test("atomic.perft"):
    perfts(Perft.atomicPerfts, Atomic, nodeLimits)
      .map(expect(_))

  test("crazyhouse.perft"):
    perfts(Perft.crazyhousePerfts, Crazyhouse, nodeLimits)
      .map(expect(_))

  test("horde.perft"):
    perfts(Perft.hordePerfts, Horde, nodeLimits)
      .map(expect(_))

  test("racingkings.perft"):
    perfts(Perft.racingkingsPerfts, RacingKings, nodeLimits)
      .map(expect(_))

  test("tricky.perft"):
    perfts(Perft.trickyPerfts, Chess960, nodeLimits)
      .map(expect(_))

  test("chess960.perft"):
    perfts(Perft.chess960, Chess960, nodeLimits)
      .map(expect(_))

  private def perfts(perfts: List[Perft], variant: Variant, nodeLimit: Long): IO[Boolean] =
    perfts.parFoldMapA(perft => IO(perftTest(perft, variant, nodeLimit)))

  private def perftTest(perft: Perft, variant: Variant, nodeLimit: Long): Boolean =
    perft
      .withLimit(nodeLimit)
      .calculate(variant)
      .forall(r => r.result === r.expected)
