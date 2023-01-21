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

  genTests("calculate ThreeCheck perfts", Perft.threeCheckPerfts, ThreeCheck, 1_000_000L)
  genTests("calculate Antichess perfts", Perft.antichessPerfts, Antichess, 1_000_000L)
  genTests("calculate Atomic perfts", Perft.atomicPerfts, Atomic, 1_000_000L)
  // TODO scalachess doesn't generate drops now
  // genTests("calculate Crazyhouse perfts", Perft.crazyhousePerfts, Crazyhouse, 1_000_000L)
  genTests("calculate Horde perfts", Perft.hordePerfts, Horde, 1_000_000L)
  genTests("calculate RacingKings perfts", Perft.racingkingsPerfts, RacingKings, 1_000_000L)
  // for the shake of time we only test the first 50 cases in random.peft, run FullRandomPerftTest.scala for all cases
  genTests("calculate random perfts", Perft.randomPerfts.take(100), Chess960, 1_000_000L)
  genTests("calculate tricky perfts", Perft.trickyPerfts, Chess960, 1_000_000L)
  genTests("calculate chess960 perfts", Perft.chess960, Chess960, 1_000_000L)
