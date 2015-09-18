package chess

import Pos.posAt

case class CheckCount(white: Int = 0, black: Int = 0) {

  def add(color: Color) = copy(
    white = white + color.fold(1, 0),
    black = black + color.fold(0, 1))

  def nonEmpty = white > 0 || black > 0

  def apply(color: Color) = color.fold(white, black)
}

case class History(
    lastMove: Option[(Pos, Pos)] = None,
    positionHashes: PositionHash = Array(),
    castles: Castles = Castles.all,
    checkCount: CheckCount = CheckCount(0, 0)) {

  def lastMoveString: Option[String] = lastMove map {
    case (p1, p2) => p1.toString + p2.toString
  }

  /**
   * Halfmove clock: This is the number of halfmoves
   * since the last pawn advance or capture.
   * This is used to determine if a draw
   * can be claimed under the fifty-move rule.
   */
  def halfMoveClock = positionHashes.size / Hash.size

  def threefoldRepetition: Boolean = halfMoveClock > 6 && {
    val positions = (positionHashes grouped Hash.size).toList
    positions.headOption match {
      case Some(Array(x, y, z)) => (positions count {
        case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
        case _                 => false
      }) >= 3
      case _ => false
    }
  }

  def fiftyMoves: Boolean = halfMoveClock >= 100

  def canCastle(color: Color) = new {
    def on(side: Side): Boolean = castles can color on side
    def any = (castles can color).any
  }

  def withoutCastles(color: Color) = copy(castles = castles without color)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  def withCastles(c: Castles) = copy(castles = c)

  def positionHashesWith(hash: PositionHash): PositionHash =
    hash ++ positionHashes

  def withLastMove(orig: Pos, dest: Pos) = copy(
    lastMove = Some((orig, dest))
  )

  def withCheck(color: Color, v: Boolean) =
    if (v) copy(checkCount = checkCount add color) else this
}

object History {

  val MoveString = """^([a-h][1-8])([a-h][1-8])$""".r

  def make(
    lastMove: Option[(Pos, Pos)],
    positionHashes: PositionHash,
    castles: Castles): History = new History(
    lastMove = lastMove,
    castles = castles,
    positionHashes = positionHashes)

  def make(
    lastMove: Option[String], // a2a4
    castles: String): History = make(
    lastMove = lastMove flatMap {
      case MoveString(a, b) => for (o ← posAt(a); d ← posAt(b)) yield (o, d)
      case _                => None
    },
    positionHashes = Array(),
    castles = Castles(castles))

  def castle(color: Color, kingSide: Boolean, queenSide: Boolean) =
    History().copy(
      castles = color match {
        case White => Castles.init.copy(
          whiteKingSide = kingSide,
          whiteQueenSide = queenSide)
        case Black => Castles.init.copy(
          blackKingSide = kingSide,
          blackQueenSide = queenSide)
      })

  def noCastle = History() withoutCastles White withoutCastles Black
}
