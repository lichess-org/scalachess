package chess

import format.Uci

// Checks received by the respective side.
case class CheckCount(white: Int = 0, black: Int = 0) {

  def add(color: Color) = copy(
    white = white + color.fold(1, 0),
    black = black + color.fold(0, 1)
  )

  def nonEmpty = white > 0 || black > 0

  def apply(color: Color) = color.fold(white, black)
}

case class UnmovedRooks(pos: Set[Pos]) extends AnyVal

object UnmovedRooks {
  val default = UnmovedRooks((Pos.whiteBackrank ::: Pos.blackBackrank).toSet)
}

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Hash.zero,
    castles: Castles = Castles.all,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks = UnmovedRooks.default
) {

  /**
    * Halfmove clock: This is the number of halfmoves
    * since the last pawn advance or capture.
    * This is used to determine if a draw
    * can be claimed under the fifty-move rule.
    */
  def halfMoveClock = math.max(0, (positionHashes.size / Hash.size) - 1)

  // generates random positionHashes to satisfy the half move clock
  def setHalfMoveClock(v: Int) =
    copy(positionHashes = History.spoofHashes(v + 1))

  def threefoldRepetition: Boolean = halfMoveClock >= 8 && {
    // compare only hashes for positions with the same side to move
    val positions = (positionHashes grouped Hash.size).sliding(1, 2).flatten.toList
    positions.headOption match {
      case Some(Array(x, y, z)) =>
        (positions count {
          case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
          case _                 => false
        }) >= 3
      case _ => false
    }
  }

  def fiftyMoves: Boolean = halfMoveClock >= 100

  def canCastle(color: Color) = new {
    def on(side: Side): Boolean = castles can color on side
    def any                     = (castles can color).any
  }

  def withoutCastles(color: Color) = copy(castles = castles without color)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  def withCastles(c: Castles) = copy(castles = c)

  def withLastMove(m: Uci) = copy(lastMove = Some(m))

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
  ): History = History(
    lastMove = lastMove flatMap Uci.apply,
    castles = Castles(castles),
    positionHashes = Array()
  )

  def castle(color: Color, kingSide: Boolean, queenSide: Boolean) =
    History(
      castles = color match {
        case White =>
          Castles.init.copy(
            whiteKingSide = kingSide,
            whiteQueenSide = queenSide
          )
        case Black =>
          Castles.init.copy(
            blackKingSide = kingSide,
            blackQueenSide = queenSide
          )
      }
    )

  def noCastle = History(castles = Castles.none)

  private def spoofHashes(n: Int): PositionHash = {
    (1 to n).toArray.flatMap { i =>
      Array((i >> 16).toByte, (i >> 8).toByte, i.toByte)
    }
  }
}
