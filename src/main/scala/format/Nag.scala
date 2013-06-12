package chess.format

// http://en.wikipedia.org/wiki/Numeric_Annotation_Glyphs
sealed abstract class Nag(
  val code: Int,
  val symbol: String
)

object Nag {
  case object Blunder extends Nag(4, "??")
  case object Mistake extends Nag(2, "?")
  case object Inaccuracy extends Nag(6, "?!")

  val all = List(Blunder, Mistake, Inaccuracy)
}
