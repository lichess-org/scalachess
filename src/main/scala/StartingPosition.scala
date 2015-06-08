package chess

case class StartingPosition(
    eco: String,
    name: String,
    fen: String) {

  def fullName = s"$eco $name"
}

object StartingPosition {

  val all = List(
    StartingPosition("B01", "Black has no pawns!", "rnbqkbnr/8/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
    StartingPosition("C02", "White has no pawns!", "rnbqkbnr/pppppppp/8/8/8/8/8/RNBQKBNR w KQkq - 0 1")
  )

  val initial = StartingPosition("---", "Initial position", format.Forsyth.initial)

  def allWithInitial = initial :: all

  private val ecoIndex: Map[String, StartingPosition] = all.map { p =>
    p.eco -> p
  }.toMap

  def byEco(eco: String): Option[StartingPosition] = ecoIndex get eco
}
