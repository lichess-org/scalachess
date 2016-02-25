package chess
package opening

object FullOpeningDB {

  private def all: Vector[FullOpening] = FullOpeningPart1.db ++ FullOpeningPart2.db

  lazy val byFen = all.map { o =>
    o.fen -> o
  }.toMap

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(moveStrs: List[String]): Option[FullOpening] =
    chess.Replay.boards(moveStrs take SEARCH_MAX_PLIES, None, variant.Standard).toOption.flatMap {
      _.zipWithIndex.drop(1).foldRight(none[FullOpening]) {
        case ((board, ply), None) =>
          val fen = format.Forsyth.exportStandardPositionTurnCastling(board, ply)
          byFen get fen
        case (_, found) => found
      }
    }
}
