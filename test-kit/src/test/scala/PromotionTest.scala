package chess

import chess.format.FullFen
import chess.variant.Standard

import scala.language.implicitConversions

class PromotionTest extends ChessTest:

  test("Not allow promotion to a king in a standard game "):
    val fen = FullFen("8/1P6/8/8/8/8/7k/1K6 w - -")
    val game = fenToGame(fen, Standard)

    val failureGame = game(Square.B7, Square.B8, Option(King)).map(_._1)

    assert(failureGame.isLeft)
