package chess

import scala.language.implicitConversions
import cats.syntax.option.*

import Pos.*
import format.Uci

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.Standard

class PT extends ChessTest:

  "pawn promotion" should {
    val board = """
  p
K      """
    val game  = Game(board, Black)
    println(game)
    "promote to a queen" in {
      game.playMove(C2, C1, Queen.some) must beGame("""

K q    """)
    }
  }
