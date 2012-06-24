package chess
package format

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String]): Valid[String] =
    pgn.trim.some filter ("" !=) fold (
      nonEmptyPgn ⇒ PgnReader(
        nonEmptyPgn,
        initialFen.fold(
          fen ⇒ Fen(fen) :: Variant(chess.Variant.Chess960.name) :: Nil,
          Nil)
      ) map (_.chronoMoves map move mkString " "),
      success(""))

  private def move(m: Move): String =
    m.orig.key + m.dest.key + m.promotion.fold(_.forsyth.toString, "")
}
