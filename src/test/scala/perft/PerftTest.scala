package chess
package perft

import org.specs2.specification.core.Fragments

import chess.format.EpdFen
import chess.variant.*

class PerftTest extends ChessTest:

  private def genTests(name: String, tests: List[Perft], variant: Variant, nodeLimit: Long): Fragments =
    name >> {
      Fragments.foreach(tests) { perft =>
        perft.id >> {
          val result = perft.withLimit(nodeLimit).calculate(variant)
          Fragments.foreach(result) { r =>
            s"${r.depth}" in {
              r.result === r.expected
            }
          }
        }
      }
    }

  // genTests("calculate ThreeCheck perfts", Perft.read("3check.perft"), ThreeCheck, 1_000_000L)
  // genTests("calculate Antichess perfts", Perft.read("antichess.perft"), Antichess, 1_000_000L)
  // genTests("calculate Atomic perfts", Perft.read("atomic.perft"), Atomic, 1_000_000L)
  // genTests("calculate Crazyhouse perfts", Perft.read("crazyhouse.perft"), Crazyhouse, 1_000_000L)
  // genTests("calculate Horde perfts", Perft.read("horde.perft"), Horde, 1_000_000L)
  // genTests("calculate RacingKings perfts", Perft.read("racingkings.perft"), RacingKings, 1_000_000L)
  // genTests("calculate random perfts", Perft.read("random.perft"), Chess960, 10_000L)
  // genTests("calculate tricky perfts", Perft.read("tricky.perft"), Chess960, 100_000L)
  genTests("calculate chess960 perfts", Perft.read("x-fen.perft"), Chess960, 10_000L)
