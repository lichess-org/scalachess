package chess
package format

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String]): Valid[String] =
    PgnReader( 
      pgn,
      initialFen.fold(Fen(_) :: Nil, Nil)
    ) map (_.chronoMoves map move mkString " ")

  private def move(m: Move): String =
    m.orig.key + m.dest.key + m.promotion.fold(_.pgn, "")
}
