package chess

import scala.language.implicitConversions

import chess.format.EpdFen
import chess.Square.*
import chess.variant.Standard

class PromotionTest extends ChessTest:

  "Not allow promotion to a king in a standard game " in:
    val fen  = EpdFen("8/1P6/8/8/8/8/7k/1K6 w - -")
    val game = fenToGame(fen, Standard).toOption.get

    val failureGame = game(Square.B7, Square.B8, Option(King)).map(_._1)

    failureGame must beLeft
