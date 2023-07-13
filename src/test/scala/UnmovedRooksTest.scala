package chess

import scala.language.implicitConversions
import Square.*
import variant.Chess960
import format.EpdFen
import chess.format.Fen
import cats.syntax.all.*

import bitboard.Board as BBoard
import bitboard.Bitboard.contains

class UnmovedRooksTest extends ChessTest:

  "UnmovedRooks with 960 initial fen" in:
    Fen
      .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.board.history.unmovedRooks) must beSome(360287970189639685L)

  "At the start, unmovedRooks == rooks" in:
    chess960Boards.map { board =>
      board.history.unmovedRooks must_== BBoard.fromMap(board.pieces).rooks
    }

  "side 1" in:
    Fen
      .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.board.history.unmovedRooks) must beSome { (ur: UnmovedRooks) =>
      ur.side(A1).flatten must beSome(QueenSide)
      ur.side(C1).flatten must beSome(KingSide)
      ur.side(A8).flatten must beSome(QueenSide)
      ur.side(C8).flatten must beSome(KingSide)
    }

  chess960Boards.mapWithIndex { (board, n) =>
    s"unmovedRooks at position number: $n" in:
      board.rooks.squares.traverse { square =>
        board.history.unmovedRooks.side(square).flatten
      } must beSome { (sides: List[Side]) =>
        sides.filter(_ == QueenSide).size must_== 2
        sides.filter(_ == KingSide).size must_== 2
      }
  }

  "rook capture rook" in:
    fenToGame(
      EpdFen("1r2qkr1/p1b1pppp/3n2n1/2p5/3B4/4PPN1/P4P1P/1RN1QKR1 w KQkq - 0 11"),
      Chess960
    ) must beRight.like { game =>
      game.playMoves(B1 -> B8) must beRight.like { case g =>
        g.board.history.unmovedRooks.contains(B1) must_== false
        g.board.history.unmovedRooks.contains(B8) must_== false
        g.board.history.unmovedRooks.contains(G1) must_== true
        g.board.history.unmovedRooks.contains(G8) must_== true
        g.board.castles must_== Castles(true, false, true, false)
      }
    }

  "capture at the conner" in:
    fenToGame(
      EpdFen("1r2k2b/p1qpp2p/1p1nn1r1/2pP4/8/1P1Q2P1/P1P1NP1P/BR2KN1R b Qq - 0 11"),
      Chess960
    ) must beRight.like { game =>
      game.playMoves(H8 -> A1) must beRight.like { case g =>
        g.board.history.unmovedRooks.contains(B1) must_== true
        g.board.history.unmovedRooks.contains(B8) must_== true
        g.board.castles must_== Castles(false, true, false, true)
      }
    }

  "capture an unmovedRook" in:
    fenToGame(
      EpdFen("brnqknr1/pppppp1p/6p1/8/3b1P1P/1P6/P1PPP1P1/BRNQKNRB b KQkq - 0 3"),
      Chess960
    ) must beRight.like { game =>
      game.playMoves(D4 -> G1) must beRight.like { case g =>
        g.board.history.unmovedRooks.contains(B1) must_== true
        g.board.history.unmovedRooks.contains(B8) must_== true
        g.board.history.unmovedRooks.contains(G1) must_== false
        g.board.history.unmovedRooks.contains(G8) must_== true
        g.board.castles must_== Castles(false, true, true, true)
      }
    }

  "An unmovedRooks moves" in:
    "white" in:
      fenToGame(EpdFen("qrnbkrbn/ppppp1pp/8/5p2/5P2/8/PPPPP1PP/QRNBKRBN w KQkq - 0 2"), Chess960) must beRight
        .like { game =>
          game.playMoves(F1 -> F2) must beRight.like { case g =>
            g.board.history.unmovedRooks.contains(B1) must_== true
            g.board.history.unmovedRooks.contains(B8) must_== true
            g.board.history.unmovedRooks.contains(F1) must_== false
            g.board.history.unmovedRooks.contains(F8) must_== true
            g.board.castles must_== Castles(false, true, true, true)
          }
        }
    "black" in:
      fenToGame(EpdFen("qrnbkrbn/ppppp1pp/8/5p2/4PP2/8/PPPP2PP/QRNBKRBN b KQkq - 0 2"), Chess960) must beRight
        .like { game =>
          game.playMoves(F8 -> F6) must beRight.like { case g =>
            g.board.history.unmovedRooks.contains(B1) must_== true
            g.board.history.unmovedRooks.contains(B8) must_== true
            g.board.history.unmovedRooks.contains(F1) must_== true
            g.board.history.unmovedRooks.contains(F8) must_== false
            g.board.castles must_== Castles(true, true, false, true)
          }
        }

  "the king moves" in:
    "white" in:
      fenToGame(EpdFen("rkrnnqbb/p1pppppp/1p6/8/8/1P6/P1PPPPPP/RKRNNQBB w KQkq - 0 2"), Chess960) must beRight
        .like { game =>
          game.playMoves(B1 -> B2) must beRight.like { case g =>
            g.board.history.unmovedRooks.contains(A1) must_== false
            g.board.history.unmovedRooks.contains(A8) must_== true
            g.board.history.unmovedRooks.contains(C1) must_== false
            g.board.history.unmovedRooks.contains(C8) must_== true
            g.board.castles must_== Castles(false, false, true, true)
          }
        }
    "black" in:
      fenToGame(
        EpdFen("rkrnnqbb/p1pppppp/1p6/8/5P2/1P6/P1PPP1PP/RKRNNQBB b KQkq - 0 2"),
        Chess960
      ) must beRight
        .like { game =>
          game.playMoves(B8 -> B7) must beRight.like { case g =>
            g.board.history.unmovedRooks.contains(A1) must_== true
            g.board.history.unmovedRooks.contains(A8) must_== false
            g.board.history.unmovedRooks.contains(C1) must_== true
            g.board.history.unmovedRooks.contains(C8) must_== false
            g.board.castles must_== Castles(true, true, false, false)
          }
        }
