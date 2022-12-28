package chess

import chess.format.EpdFen
import chess.variant.Chess960
import org.specs2.specification.core.Fragments

class PerftTest extends ChessTest:

  private def genTests(name: String, tests: List[PerftTestCase]): Fragments =
    name should {
      Fragments.foreach(tests) { pts =>
        pts.id should {
          val result = pts.calculate()
          Fragments.foreach(result) { r =>
            s"${r.depth}" in {
              r.result must be equalTo r.expected
            }
          }
        }
      }
    }

  genTests("calculate chess960 perfts", PerftTestCase.chess960)
  genTests("calculate tricky perfts", PerftTestCase.tricky)
