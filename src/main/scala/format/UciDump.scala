package chess
package format

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String): Valid[String] = 
    PgnReader(pgn) map (_.chronoMoves map move mkString " ")

  private def move(m: Move): String =
    m.orig.key + m.dest.key + m.promotion.fold(_.pgn, "")
}
