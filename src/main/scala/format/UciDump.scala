package chess
package format

import chess.variant.Variant

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(moves: List[String], initialFen: Option[String], variant: Variant): Valid[List[String]] =
    moves.isEmpty.fold(
      success(Nil),
      Replay(moves, initialFen, variant) map apply
    )

  def move(variant: Variant)(m: Move): String = m.castle.fold(
    m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
  ) {
      case ((kf, kt), (rf, rt)) if kf == kt || !variant.standardInitialPosition => kf.key + rf.key
      case ((kf, kt), _) => kf.key + kt.key
    }
}
