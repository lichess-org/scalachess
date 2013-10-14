package chess
package format

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(pgn: String, initialFen: Option[String], variant: Variant): Valid[List[String]] =
    pgn.trim.isEmpty.fold(
      success(Nil),
      Replay(pgn, initialFen, variant) map apply
    )

  def move(variant: Variant)(m: Move): String = m.castle.fold(
    m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
  ) {
      case ((kf, kt), (rf, rt)) if kf == kt || !variant.standard ⇒ kf.key + rf.key
      case ((kf, kt), _)                                         ⇒ kf.key + kt.key
    }
}
