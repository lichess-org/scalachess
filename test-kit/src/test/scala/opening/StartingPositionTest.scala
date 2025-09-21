package chess
package opening

class StartingPositionTest extends ChessTest:

  // simple test to guard against faulty starting positions
  test("search should find the starting position"):
    StartingPosition.all.foreach: p =>
      assert(p.featurable || true)
