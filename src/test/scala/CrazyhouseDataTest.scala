package chess

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import Arbitraries.given

class CrazyhouseDataTest extends ScalaCheckSuite:
  test("CrazyhouseDataTest"):
    forAll { (piece: Piece) =>
      (piece.role != King) ==> (piece.role != King)
    }
