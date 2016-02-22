package chess

object FullOpeningDB {

  lazy val byFen = all.map { o =>
    o.fen -> o
  }.toMap

  val all: List[FullOpening] =
    FullOpeningPart1.db ++ FullOpeningPart2.db ++ FullOpeningPart3.db
}
