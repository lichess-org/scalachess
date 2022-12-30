package chess
package perft

import org.specs2.specification.core.Fragments

import chess.format.EpdFen
import chess.variant.*

class PerftTest extends ChessTest:

  private def genTests(name: String, tests: List[Perft], variant: Variant): Fragments =
    name >> {
      Fragments.foreach(tests) { pts =>
        pts.id >> {
          val result = pts.calculate(variant)
          Fragments.foreach(result) { r =>
            s"${r.depth}" in {
              r.result === r.expected
            }
          }
        }
      }
    }

  genTests("calculate ThreeCheck perfts", Perft.read("3check.perft"), ThreeCheck)
  genTests("calculate Antichess perfts", Perft.read("antichess.perft"), Antichess)
  genTests("calculate Atomic perfts", Perft.read("atomic.perft"), Atomic)
  genTests("calculate Crazyhouse perfts", Perft.read("crazyhouse.perft"), Crazyhouse)
  genTests("calculate Horde perfts", Perft.read("horde.perft"), Horde)
  genTests("calculate RacingKings perfts", Perft.read("racingKings.perft"), RacingKings)
  genTests("calculate chess960 perfts", Perft.read("random.perft"), Chess960)
  genTests("calculate tricky perfts", Perft.read("tricky.perft"), Chess960)
