package chess

import scala.language.implicitConversions
import cats.syntax.option.*

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.Standard

class PromotionTest extends ChessTest:

  "Not allow promotion to a king in a standard game " in {
    val fen  = EpdFen("8/1P6/8/8/8/8/7k/1K6 w - -")
    val game = fenToGame(fen, Standard)

    val failureGame = game flatMap (_.apply(Pos.B7, Pos.B8, Option(King))) map (_._1)

    failureGame must beInvalid
  }
