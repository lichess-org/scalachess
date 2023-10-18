package chess

import Square.*

class PlayOneMoveTest extends ChessTest:

  test("only process things once"):
    assert(makeGame.playMoves(E2 -> E4).isRight)
