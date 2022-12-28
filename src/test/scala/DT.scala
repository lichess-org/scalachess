package chess

import Pos.*
import variant.{ Antichess, Atomic, Crazyhouse, Standard, ThreeCheck }
import chess.format.{ EpdFen, Fen, Uci }
import chess.format.pgn.SanStr

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*
import cats.syntax.option.*

import chess.format.EpdFen
import chess.variant.Crazyhouse
import chess.format.pgn.SanStr

class DT extends ChessTest:

  "Crazyhouse" should {

    val hash = Hash(8)

    "be consistent when king is captured in antichess" in {
      val fen           = EpdFen("rnbqkb1r/ppp1pppp/3p1n2/1B6/8/4P3/PPPP1PPP/RNBQK1NR w KQkq - 2 3")
      val situation     = Fen.read(Antichess, fen).get
      val move          = situation.move(Pos.B5, Pos.E8, None).toOption.get
      val hashAfterMove = hash(move.situationAfter)

      // 3. BxK
      val fenAfter       = EpdFen("rnbqBb1r/ppp1pppp/3p1n2/8/8/4P3/PPPP1PPP/RNBQK1NR b KQkq - 0 3")
      val situationAfter = Fen.read(Antichess, fenAfter).get
      val hashAfter      = hash(situationAfter)

      hashAfterMove mustEqual hashAfter
    }

  }
