package chess

import scala.language.implicitConversions
import Pos.*
import variant.FromPosition
import variant.Chess960
import format.EpdFen
import chess.format.Fen
import cats.syntax.all.*

class UnmovedRooksTest extends ChessTest:

  "UnmovedRooks with initial fen" in {
    Fen
      .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.board.history.unmovedRooks) must beSome(360287970189639685L)
  }

  "At the start, unmovedRooks == rooks" in {
    chess960Boards.map { board =>
      board.history.unmovedRooks must_== bitboard.Board.fromMap(board.pieces).rooks
    }
  }

  "UnmovedRooks.side 1" in {
    Fen
      .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.board.history.unmovedRooks) must beSome { (ur: UnmovedRooks) =>
      ur.side(A1).flatten must beSome(QueenSide)
      ur.side(C1).flatten must beSome(KingSide)
      ur.side(A8).flatten must beSome(QueenSide)
      ur.side(C8).flatten must beSome(KingSide)
    }
  }

  chess960Boards.foreach { board =>
    "unmovedRooks.side" in {
      board.rooks.occupiedSquares.traverse { pos =>
        board.history.unmovedRooks.side(pos).flatten
      } must beSome { (sides: List[Side]) =>
        sides.filter(_ == QueenSide).size must_== 2
        sides.filter(_ == KingSide).size must_== 2
      }
    }
  }

