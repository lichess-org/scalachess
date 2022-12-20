package chess
package bitboard

import munit.FunSuite
import org.lichess.compression.game.MoveList

class StandardMovesGeneratorTest extends FunSuite:

  import scala.language.implicitConversions
  given Conversion[Pos, Int] = _.value

  import StandardMovesGenerator.*
  import Helpers.*


  test("genEnPassant 1") {
    val b        = Board.empty.copy(pawns = 0x7000000000L, white = 0x5000000000L, black = 0x2000000000L)
    val ep       = Pos(File.F, Rank.Sixth)
    val fen      = Fen(b, State(White, Some(ep), Bitboard.corners, 5, 10))
    val moves    = fen.genEnPassant(ep)
    val expected = Set(Move.EnPassant(Pos.at(36).get, ep), Move.EnPassant(Pos.at(38).get, ep))
    assertEquals(moves.toSet, expected.toSet)
  }

  test("genPawn for standard position") {
    val fen           = Fen.standard
    val targets       = ~fen.us
    val moves         = fen.genPawn(targets)
    val moveList      = MoveList()
    val expectedMoves = fen.cBoard.genPawn(targets, moveList)
    assertMoves(moves, moveList)
  }

  test("genPawn with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genPawn(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genPawn(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genKnight with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genKnight(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genKnight(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genBishop with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genBishop(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genBishop(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genRook with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genRook(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genRook(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genQueen with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genQueen(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genQueens(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genNonKing with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genNonKing(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genNonKing(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genSafeKing with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val king          = fen.ourKing.get
      val targets       = ~fen.us
      val moves         = fen.genSafeKing(king, targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genSafeKing(king, targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genCastling with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val king          = fen.ourKing.get
      val targets       = ~fen.us
      val moves         = fen.genCastling(king)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genCastling(king, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genEvasion with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val king          = fen.ourKing.get
      val checkers      = fen.checkers.get
      val moves         = fen.genEvasions(king, checkers)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genEvasions(king, checkers, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("legalMoves with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val moves         = fen.generate
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.legalMoves(moveList)
      assertMoves(moves, moveList)
    }
  }

  private def assertMoves(moves: List[Move], moveList: MoveList) =
    assertEquals(moves.length, moveList.size)
    assertEquals(moves.map(_.uci).toSet, moveList.uciSet)
