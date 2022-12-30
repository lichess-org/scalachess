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

  // Tests all perft scenario on random.perft with node limitation is 10_000
  test("random.perft") {
    Perft.read("random.perft")
      .parTraverse(perft => IO(genTests(perft, Chess960, 10_000L)))
      .map(_.forall(identity))
      .map(assert(_))
  }

  private def genTests(perft: Perft, variant: Variant, nodeLimit: Long): Boolean =
      perft.withLimit(nodeLimit).calculate(variant)
        .forall(r => r.result === r.expected)
