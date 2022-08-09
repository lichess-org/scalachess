package chess

import format.Uci

// Checks received by the respective side.
case class CheckCount(red: Int = 0, black: Int = 0) {

  def add(color: Color) =
    copy(
      red = red + color.fold(1, 0),
      black = black + color.fold(0, 1)
    )

  def nonEmpty = red > 0 || black > 0

  def apply(color: Color) = color.fold(red, black)
}

case class UnmovedRooks(pos: Set[Pos]) extends AnyVal

object UnmovedRooks {
  val default = UnmovedRooks((Pos.redBackrank ::: Pos.blackBackrank).toSet)
}

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    castles: Castles = Castles.all,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks = UnmovedRooks.default,
    halfMoveClock: Int = 0
) {
  def setHalfMoveClock(v: Int) = copy(halfMoveClock = v)

  private def isRepetition(times: Int) =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      // compare only hashes for positions with the same side to move
      val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList
      positions.headOption match {
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _                 => false
          }) >= times
        case _ => times <= 1
      }
    }

  def threefoldRepetition = isRepetition(3)

  def fivefoldRepetition = isRepetition(5)

  def canCastle(color: Color) = castles can color

  def withoutCastles(color: Color) = copy(castles = castles without color)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  def withCastles(c: Castles) = copy(castles = c)

  def withLastMove(m: Uci) = copy(lastMove = Option(m))

  def withCheck(color: Color, v: Boolean) =
    if (v) copy(checkCount = checkCount add color) else this

  def withCheckCount(cc: CheckCount) = copy(checkCount = cc)

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastMove.fold("-")(_.uci)} ${positions.map(Hash.debug).mkString(" ")}"
  }
}

object History {

  def make(
      lastMove: Option[String], // a2a4
      castles: String
  ): History =
    History(
      lastMove = lastMove flatMap Uci.apply,
      castles = Castles(castles),
      positionHashes = Array()
    )

  def castle(color: Color, kingSide: Boolean, queenSide: Boolean) =
    History(
      castles = color match {
        case Red =>
          Castles.init.copy(
            redKingSide = kingSide,
            redQueenSide = queenSide
          )
        case Black =>
          Castles.init.copy(
            blackKingSide = kingSide,
            blackQueenSide = queenSide
          )
      }
    )

  def noCastle = History(castles = Castles.none)
}
