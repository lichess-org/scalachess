package chess

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*

import chess.format.EpdFen
import chess.variant.Crazyhouse
import chess.format.pgn.SanStr

class DT extends ChessTest:

  "Crazyhouse" should {

    "Allow castling with touching kings and rook shielding final attack" in {
      val position = EpdFen("8/8/8/8/8/8/4k3/R3K2r w Q - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Pos.E1, Pos.C1))

      newGame must beValid.like { case game =>
        game.board(Pos.C1) must beEqualTo(White.king.some)
        game.board(Pos.D1) must beEqualTo(White.rook.some)
      }
    }

    // "Must be a stalemate if a king could usually take a piece, but can't because it would explode" in {
    //   val positionFen = EpdFen("k7/8/1R6/8/8/8/8/5K2 w - -")
    //   val maybeGame   = fenToGame(positionFen, Atomic)
    //
    //   val gameWin = maybeGame flatMap (_.playMoves((Pos.B6, Pos.B7)))
    //
    //   gameWin must beValid.like { case game =>
    //     println(game.situation.moves)
    //     game.situation.end must beTrue
    //     game.situation.staleMate must beTrue
    //   }
    // }

    // "Verify that a king can move into what would traditionally be check when touching the opponent king" in {
    //   val position = EpdFen("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
    //   val game     = fenToGame(position, Atomic)
    //
    //   val successGame = game flatMap (_.playMoves((Pos.F6, Pos.F5)))
    //
    //   successGame must beValid
    // }

    // "Not allow a king to capture a piece" in {
    //   val fenPosition = EpdFen("8/8/8/1k6/8/8/8/1Kr5 w - -")
    //   val maybeGame   = fenToGame(fenPosition, Atomic)
    //
    //   val errorGame = maybeGame flatMap (_.playMoves((Pos.B1, Pos.C1)))
    //
    //   errorGame must beInvalid("Piece on b1 cannot move to c1")
    // }

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
