package chess

import munit.ScalaCheckSuite

class OutcomeTest extends ScalaCheckSuite:

  import Outcome.*

  test("standard outcomes"):
    assertEquals(fromResult("1-0"), Some(white))
    assertEquals(fromResult("0-1"), Some(black))
    assertEquals(fromResult("1/2-1/2"), Some(draw))

  test("silly format outcomes"):
    assertEquals(fromResult("+_-"), Some(white))
    assertEquals(fromResult("0:1"), Some(black))
    assertEquals(fromResult("½-½"), Some(draw))
    assertEquals(fromResult("WHITEWIN"), Some(white))

  test("points"):
    assertEquals(pointsFromResult("1-0"), Some(ByColor[Points](1, 0)))
    assertEquals(pointsFromResult("0-1"), Some(ByColor[Points](0, 1)))
    assertEquals(pointsFromResult("—:—"), Some(ByColor[Points](0, 0)))
    assertEquals(pointsFromResult("1/2_0"), Some(ByColor[Points](0.5, 0)))
    assertEquals(pointsFromResult("0-½"), Some(ByColor[Points](0, 0.5)))
