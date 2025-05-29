package chess
import macros.*

class CanPlayTest extends ChessTest:

  test("validate"):
    val position = Position.standard
    val moves    = List(uci"e2e4", uci"e7e5", uci"g1f3", uci"b8c6")
    val result   = position.validate(moves)
    assert(result.isRight)
