package chess
package perft

import cats.syntax.all.*
import weaver.*

import chess.variant.*
import cats.effect.IO
import cats.kernel.Monoid

object FullPerftTest extends SimpleIOSuite:

  given Monoid[Boolean] with
    def empty                           = true
    def combine(x: Boolean, y: Boolean) = x && y

  val nodeLimits = 1_000L

  test("random.perft"):
    perfts(Perft.randomPerfts, Chess960, 10_000L)
      .map(assert(_))

  test("threeCheck.perft"):
    perfts(Perft.threeCheckPerfts, ThreeCheck, nodeLimits)
      .map(assert(_))

  test("antichess.perft"):
    perfts(Perft.antichessPerfts, Antichess, nodeLimits)
      .map(assert(_))

  test("atomic.perft"):
    perfts(Perft.atomicPerfts, Atomic, nodeLimits)
      .map(assert(_))

  test("crazyhouse.perft"):
    perfts(Perft.crazyhousePerfts, Crazyhouse, nodeLimits)
      .map(assert(_))

  test("horde.perft"):
    perfts(Perft.hordePerfts, Horde, nodeLimits)
      .map(assert(_))

  test("racingkings.perft"):
    perfts(Perft.racingkingsPerfts, RacingKings, nodeLimits)
      .map(assert(_))

  test("tricky.perft"):
    perfts(Perft.trickyPerfts, Chess960, nodeLimits)
      .map(assert(_))

  test("chess960.perft"):
    perfts(Perft.chess960, Chess960, nodeLimits)
      .map(assert(_))

  private def perfts(perfts: List[Perft], variant: Variant, nodeLimit: Long): IO[Boolean] =
    perfts.parFoldMapA(perft => IO(perftTest(perft, variant, nodeLimit)))

  private def perftTest(perft: Perft, variant: Variant, nodeLimit: Long): Boolean =
    perft
      .withLimit(nodeLimit)
      .calculate(variant)
      .forall(r => r.result === r.expected)
