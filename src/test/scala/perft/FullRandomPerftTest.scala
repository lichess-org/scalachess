package chess
package perft

import cats.syntax.all.*
import cats.effect.syntax.all.*
import weaver.*

import chess.format.EpdFen
import chess.variant.*
import cats.effect.IO
import cats.kernel.Monoid

object FullRandomPerftTest extends SimpleIOSuite:

  given Monoid[Boolean] with
    def empty = true
    def combine(x: Boolean, y: Boolean) = x && y

  // Tests all perft scenario on random.perft with node limitation is 10_000
  test("random.perft") {
    Perft.randomPerfts
      .parFoldMapA(perft => IO(genTests(perft, Chess960, 10_000L)))
      .map(assert(_))
  }

  private def genTests(perft: Perft, variant: Variant, nodeLimit: Long): Boolean =
      perft.withLimit(nodeLimit).calculate(variant)
        .forall(r => r.result === r.expected)
