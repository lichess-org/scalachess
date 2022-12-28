package chess

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*

import chess.format.EpdFen
import chess.variant.Crazyhouse
import chess.format.pgn.SanStr

class DT extends ChessTest:

  "Crazyhouse" should {

    "Not allow a king to capture a piece" in {
      val fenPosition = EpdFen("8/8/8/1k6/8/8/8/1Kr5 w - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val errorGame = maybeGame flatMap (_.playMoves((Pos.B1, Pos.C1)))

      errorGame must beInvalid("Piece on b1 cannot move to c1")
    }

    // "The game must end with the correct winner when a king explodes in the perimeter of a captured piece" in {
    //   val fenPosition = EpdFen("rnb1kbnr/ppp1pppp/8/3q4/8/7P/PPPP1PP1/RNBQKBNR b KQkq -")
    //   val maybeGame   = fenToGame(fenPosition, Atomic)
    //
    //   val gameWin = maybeGame flatMap (_.playMoves((Pos.D5, Pos.D2)))
    //
    //   gameWin must beValid.like { case winningGame =>
    //     winningGame.situation.end must beTrue
    //     winningGame.situation.variantEnd must beTrue
    //     winningGame.situation.winner must beSome(Black)
    //   }
    // }
    //
    // "Must explode surrounding non pawn pieces on capture" in {
    //   val fenPosition     = EpdFen("rnbqkbnr/1ppppp1p/p5p1/8/8/1P6/PBPPPPPP/RN1QKBNR w KQkq -")
    //   val maybeGame       = fenToGame(fenPosition, Atomic)
    //   val explodedSquares = List(Pos.H8, Pos.G8)
    //   val intactPawns     = List(Pos.F7, Pos.G6, Pos.H7)
    //
    //   val explosionGame = maybeGame flatMap (_.playMoves((Pos.B2, Pos.H8)))
    //   println(s"explosionGame $explosionGame")
    //
    //   explosionGame must beValid.like { case game =>
    //     println(s"game $game")
    //     explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
    //     intactPawns.forall(pos => game.situation.board(pos).isDefined) must beTrue
    //   }
    // }

    // "destinations prod bug on game VVXRgsQT" in {
    //   import chess.Pos.*
    //   chess
    //     .Game(
    //       Crazyhouse.some,
    //       EpdFen("r2q1b1r/p2k1Ppp/2p2p2/4p3/P2nP2n/3P1PRP/1PPB1K1q~/RN1Q1B2/Npb w - - 40 21").some
    //     )
    //     .situation
    //     .destinations must_== Map(
    //     F2 -> List(E3, E1),
    //     G3 -> List(G2),
    //     F1 -> List(G2)
    //   )
    // }

  }
