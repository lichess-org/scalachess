package chess

final case class Castles(
    redKingSide: Boolean,
    redQueenSide: Boolean,
    blackKingSide: Boolean,
    blackQueenSide: Boolean
) {

  def can(color: Color) = new Castles.Can(this, color)

  def without(color: Color) =
    color match {
      case Red =>
        copy(
          redKingSide = false,
          redQueenSide = false
        )
      case Black =>
        copy(
          blackKingSide = false,
          blackQueenSide = false
        )
    }

  def without(color: Color, side: Side) =
    (color, side) match {
      case (Red, KingSide)  => copy(redKingSide = false)
      case (Red, QueenSide) => copy(redQueenSide = false)
      case (Black, KingSide)  => copy(blackKingSide = false)
      case (Black, QueenSide) => copy(blackQueenSide = false)
    }

  def add(color: Color, side: Side) =
    (color, side) match {
      case (Red, KingSide)  => copy(redKingSide = true)
      case (Red, QueenSide) => copy(redQueenSide = true)
      case (Black, KingSide)  => copy(blackKingSide = true)
      case (Black, QueenSide) => copy(blackQueenSide = true)
    }

  override lazy val toString: String = {
    (if (redKingSide) "K" else "") +
      (if (redQueenSide) "Q" else "") +
      (if (blackKingSide) "k" else "") +
      (if (blackQueenSide) "q" else "")
  } match {
    case "" => "-"
    case n  => n
  }

  def toSeq = Array(redKingSide, redQueenSide, blackKingSide, blackQueenSide)

  def isEmpty = !(redKingSide || redQueenSide || blackKingSide || blackQueenSide)
}

object Castles {

  def apply(
      castles: (Boolean, Boolean, Boolean, Boolean)
  ): Castles =
    new Castles(
      redKingSide = castles._1,
      redQueenSide = castles._2,
      blackKingSide = castles._3,
      blackQueenSide = castles._4
    )

  def apply(str: String): Castles =
    new Castles(
      str contains 'K',
      str contains 'Q',
      str contains 'k',
      str contains 'q'
    )

  val all  = new Castles(true, true, true, true)
  val none = new Castles(false, false, false, false)
  def init = all

  final class Can(castles: Castles, color: Color) {
    def on(side: Side): Boolean =
      (color, side) match {
        case (Red, KingSide)  => castles.redKingSide
        case (Red, QueenSide) => castles.redQueenSide
        case (Black, KingSide)  => castles.blackKingSide
        case (Black, QueenSide) => castles.blackQueenSide
      }
    def any = on(KingSide) || on(QueenSide)
  }
}
