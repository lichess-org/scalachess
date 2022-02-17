package chess

import cats.syntax.option._
import chess.Pos._
import chess.format.FEN
import chess.variant.NewChess1

class NewChess1VariantTest extends ChessTest {

  "NewChess1" should {

    "nothing to drop" in {
      val fenPosition = FEN("3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25")
      val game = {
        fenToGame(fenPosition, NewChess1).toOption.get
      }.updateBoard { b =>
        b.withNewChess1Data(
          NewChess1.Data(
            pockets = NewChess1.Pockets(
              NewChess1.Pocket(Nil),
              NewChess1.Pocket(Nil)
            )
          )
        )
      }
      game.situation.checkMate must beTrue
      game.situation.opponentHasInsufficientMaterial must beFalse
    }

    "pieces to drop, in vain" in {
      val fenPosition = FEN("3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25")
      val game = {
        fenToGame(fenPosition, NewChess1).toOption.get
      }.updateBoard { b =>
        b.withNewChess1Data(
          NewChess1.Data(
            pockets = NewChess1.Pockets(
              NewChess1.Pocket(Duke :: Nil),
              NewChess1.Pocket(Duke :: Nil)
            )
          )
        )
      }
      game.situation.checkMate must beTrue
      game.situation.opponentHasInsufficientMaterial must beFalse
    }

    "autodraw" in {
      "tons of pointless moves but shouldn't apply 50-moves" in {
        val moves = List.fill(30)(List(B1 -> C3, B8 -> C6, C3 -> B1, C6 -> B8))
        Game(NewChess1).playMoves(moves.flatten: _*) must beValid.like { case g =>
          g.board.variant.fiftyMoves(g.board.history) must beFalse
          g.board.autoDraw must beTrue // fivefold repetition
        }
      }
      "draw when only kings left" in {
        val fenPosition = FEN("k6K/8/8/8/8/8/8/8 w - - 0 25")
        val game = {
          fenToGame(fenPosition, NewChess1).toOption.get
        }
        game.situation.autoDraw must beTrue
        game.situation.end must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }
    "prod 50 1 accumulate hash" in {
      val gameMoves = format.pgn.Fixtures.prod1newchess1.map {
        _.split(' ').toList
      }
      def runOne(moves: List[String]) =
        Replay.gameMoveWhileValid(moves, format.Forsyth.initial, NewChess1)
      def hex(buf: Array[Byte]): String = buf.map("%02x" format _).mkString
      val g                             = gameMoves.map(runOne)
      g.exists(_._3.nonEmpty) must beFalse
      g.head._2.last._1.situation.checkMate must beTrue
      val m8  = java.security.MessageDigest getInstance "MD5"
      val m16 = java.security.MessageDigest getInstance "MD5"
      val h   = new Hash(16)
      g.foreach(_._2.foreach(x => { val ph = h(x._1.situation); m8.update(ph.slice(0, 8)); m16.update(ph) }))
      hex(m8.digest) must beEqualTo("969528f3aa8b4d41522023d5f79dc4ff")
      hex(m16.digest) must beEqualTo("2384a46a9e0469be1cdf487f4c741e02")
    }
  }
}
