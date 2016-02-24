package chess

object FullOpeningDB {

  private def all: Vector[FullOpening] = FullOpeningPart1.db ++ FullOpeningPart2.db

  lazy val byFen = all.map { o =>
    o.fen -> o
  }.toMap

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(moveStrs: List[String]): Option[FullOpening] =
    Replay.boards(moveStrs take SEARCH_MAX_PLIES, None, variant.Standard).toOption.flatMap {
      _.foldLeft(none[FullOpening]) {
        case (None, board) => None
        case (found, _)    => found
      }
    }
}
