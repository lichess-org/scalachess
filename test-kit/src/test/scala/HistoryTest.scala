package chess

class ThreefoldRepetitionTest extends ChessTest:

  def toHash(a: Int) = PositionHash(Hash(a << 8))
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
  test("no false threefold (https://lichess.org/game/qEtfmiml)"):
    val pgn = format.pgn.PgnStr(
      "e4 c6 Nf3 d5 c3 dxe4 Ng5 e5 Nxe4 Nf6 d3 Nxe4 dxe4 Qxd1+ Kxd1 Bc5 Be3 Bg4+ Be2 Bxe2+ Kxe2 Bxe3 Kxe3 Na6 Nd2 O-O-O Ke2 Rhe8 Rad1 Nc5 Rhe1 g6 Kf1 f5 f3 fxe4 fxe4 Rf8+ Kg1 g5 Nc4 Rxd1 Rxd1 g4 Nxe5 Rg8 b4 Nxe4 Rd7 Re8 Rxh7 Rxe5 Rh4 Rg5 h3 Nxc3 hxg4 Nxa2 Rh3 Nxb4 Kf2 Rxg4 g3 Rd4 Kf3 c5 g4 c4 g5 c3 g6 c2 g7 Rd8 Rh8 c1=Q Rxd8+ Kxd8 g8=Q+ Kc7 Qf7+ Kb6 Qf6+ Qc6+ Qxc6+ Kxc6 Ke4 Kc5 Ke3 Kc4 Kd2 Kb3 Ke3 Kb2 Kd4 a5 Kc5 a4 Kxb4 a3 Kb5 a2 Kb6 a1=R Kxb7 Ra5 Kb6 Rg5 Kc6 Kc3 Kd6 Kd4 Ke6 Rd5 Kf6 Ke4 Ke6 Kd4 Kf6 Ra5 Ke6 Kc5 Ke5 Kc4+ Ke4 Kb5 Kd5 Kb4+ Kd4 Rb5 Ke4 Rc5 Kd4 Kb5 Ke4 Rc6 Kd5 Kb6 Ke5 Rc4 Kd5 Kb5 Ke5 Kc5 Kf5 Kd5 Kg5 Re4 Kf5 Kd4 Kg5 Ke5 Kh5 Kf5 Kh6 Rg4 Kh5"
    )
    val replay = Replay.mainline(pgn).flatMap(_.valid).toOption.get
    assertEquals(replay.moves.size, 157)
    assert(replay.moves.forall(!_.after.threefoldRepetition))

class HalfMoveClockTest extends ChessTest:

  test("set 0"):
    assertEquals(defaultHistory().setHalfMoveClock(HalfMoveClock.initial).halfMoveClock, HalfMoveClock(0))
  test("set 5"):
    assertEquals(defaultHistory().setHalfMoveClock(HalfMoveClock(5)).halfMoveClock, HalfMoveClock(5))
