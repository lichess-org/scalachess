package chess

import munit.FunSuite

class StatusTest extends FunSuite:

  test("finished with winner"):
    assertEquals(
      Status.finishedWithWinner,
      List(Status.Mate, Status.Resign, Status.Cheat, Status.VariantEnd)
    )
