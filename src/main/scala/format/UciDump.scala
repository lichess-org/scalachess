package chess
package format

import scalaz.Validation.FlatMap._
import scalaz.Validation.{ success, failure }

import chess.variant.Variant

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(moves: Seq[String], initialFen: Option[String], variant: Variant): Valid[List[String]] =
    moves.isEmpty.fold(
      success(Nil),
      Replay(moves, initialFen, variant) flatMap {
        case pgn.Reader.Result.Incomplete(_, errs) => failure(errs)
        case pgn.Reader.Result.Complete(replay) => success(apply(replay))
      }
    )

  def move(variant: Variant)(mod: MoveOrDrop): String = mod match {
    case Left(m) => m.castle.fold(m.toUci.uci) {
      case ((kf, kt), (rf, _)) if kf == kt || variant.chess960 || variant.fromPosition => kf.key + rf.key
      case ((kf, kt), _) => kf.key + kt.key
    }
    case Right(d) => d.toUci.uci
  }
}
