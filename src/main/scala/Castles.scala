package chess

case class Castles(
    whiteKingSide: Boolean,
    whiteQueenSide: Boolean,
    blackKingSide: Boolean,
    blackQueenSide: Boolean) {

  def can(color: Color) = new {
    def on(side: Side): Boolean = (color, side) match {
      case (White, KingSide)  => whiteKingSide
      case (White, QueenSide) => whiteQueenSide
      case (Black, KingSide)  => blackKingSide
      case (Black, QueenSide) => blackQueenSide
    }
    def any = on(KingSide) || on(QueenSide)
  }

  def without(color: Color) = color match {
    case White => copy(
      whiteKingSide = false,
      whiteQueenSide = false)
    case Black => copy(
      blackKingSide = false,
      blackQueenSide = false)
  }

  def without(color: Color, side: Side) = (color, side) match {
    case (White, KingSide)  => copy(whiteKingSide = false)
    case (White, QueenSide) => copy(whiteQueenSide = false)
    case (Black, KingSide)  => copy(blackKingSide = false)
    case (Black, QueenSide) => copy(blackQueenSide = false)
  }

  override lazy val toString: String = {
    (if (whiteKingSide) "K" else "") +
      (if (whiteQueenSide) "Q" else "") +
      (if (blackKingSide) "k" else "") +
      (if (blackQueenSide) "q" else "")
  } match {
    case "" => "-"
    case n  => n
  }

  def toList = List(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)

  def isEmpty = !(whiteKingSide || whiteQueenSide || blackKingSide || blackQueenSide)
}

object Castles {

  def apply(
    castles: (Boolean, Boolean, Boolean, Boolean)): Castles = new Castles(
    whiteKingSide = castles._1,
    whiteQueenSide = castles._2,
    blackKingSide = castles._3,
    blackQueenSide = castles._4)

  def apply(str: String): Castles = new Castles(
    str contains 'K',
    str contains 'Q',
    str contains 'k',
    str contains 'q')

  val all = new Castles(true, true, true, true)
  val none = new Castles(false, false, false, false)
  def init = all
}

