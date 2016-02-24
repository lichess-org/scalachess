package chess

object FullOpeningDB {

  lazy val byFen = all.map { o =>
    o.fen -> o
  }.toMap

  val all: List[FullOpening] = FullOpeningPart1.db ++ FullOpeningPart2.db

  // assumes standard initial FEN and variant
  def find(moveStrs: List[String]): Option[FullOpening] =
    Replay.boards(moveStrs, None, variant.Standard).toOption.flatMap {
      _.foldLeft(none[FullOpening]) {
        case (None, board) => None
        case (found, _)    => found
      }
    }
}
