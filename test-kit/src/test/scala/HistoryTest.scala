package chess

class ThreefoldRepetitionTest extends ChessTest:

  def toHash(a: Int)                    = PositionHash(Hash(a << 8))
  def makeHistory(positions: List[Int]) =
    (positions
      .map(toHash))
      .foldLeft(defaultHistory()): (history, hash) =>
        history.copy(positionHashes = hash.combine(history.positionHashes))

  test("empty history"):
    assert(!defaultHistory().threefoldRepetition)
  test("not 3 same elements"):
    val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 55))
    assert(!history.threefoldRepetition)
  test("not 3 elements same to the last one"):
    val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 2, 55))
    assert(!history.threefoldRepetition)
  test("positive"):
    val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 2))
    assert(history.threefoldRepetition)

class HalfMoveClockTest extends ChessTest:

  test("set 0"):
    assertEquals(defaultHistory().setHalfMoveClock(HalfMoveClock.initial).halfMoveClock, HalfMoveClock(0))
  test("set 5"):
    assertEquals(defaultHistory().setHalfMoveClock(HalfMoveClock(5)).halfMoveClock, HalfMoveClock(5))
