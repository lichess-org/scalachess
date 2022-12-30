package chess
package perft

import chess.format.EpdFen
import chess.variant.Chess960
import org.specs2.specification.core.Fragments

class PerftTest extends ChessTest:

  private def genTests(name: String, tests: List[Perft]): Fragments =
    name >> {
      Fragments.foreach(tests) { pts =>
        pts.id >> {
          val result = pts.calculate()
          Fragments.foreach(result) { r =>
            s"${r.depth}" in {
              r.result === r.expected
            }
          }
        }
      }
    }

  // genTests("calculate chess960 perfts", Perft.chess960)
  genTests("calculate tricky perfts", Perft.tricky)
