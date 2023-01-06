package chess

import format.Uci
import cats.kernel.Monoid
import bitboard.Bitboard
// import Castles.*

// Checks received by the respective side.
case class CheckCount(white: Int = 0, black: Int = 0):

  def add(color: Color) =
    copy(
      white = white + color.fold(1, 0),
      black = black + color.fold(0, 1)
    )

  def nonEmpty = white > 0 || black > 0

  def apply(color: Color) = color.fold(white, black)

// color
case class History(
    lastMove: Option[Uci] = None,
    // turn: Color,
    positionHashes: PositionHash = Monoid[PositionHash].empty,
    castles: Castles = Castles.corners,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks = UnmovedRooks.corners,
    halfMoveClock: HalfMoveClock = HalfMoveClock(0),
    // fullMoves: FullMoveNumber = FullMoveNumber(0), // do we need it nows? => no
    // possible en-passant square
    epSquare: Option[Pos] = None
):

  def setHalfMoveClock(v: HalfMoveClock) = copy(halfMoveClock = v)

  private def isRepetition(times: Int) =
    positionHashes.value.length > (times - 1) * 4 * Hash.size && {
      // compare only hashes for positions with the same side to move
      val positions = positionHashes.value.sliding(Hash.size, 2 * Hash.size).toList
      positions.headOption match
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _                 => false
          }) >= times
        case _ => times <= 1
    }

  inline def threefoldRepetition = isRepetition(3)

  inline def fivefoldRepetition = isRepetition(5)

  inline def canCastle(inline color: Color) = castles can color

  inline def withoutCastles(inline color: Color) = copy(castles = castles without color)

  inline def withoutAnyCastles = copy(castles = Castles.none)

  inline def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  inline def withCastles(inline c: Castles) = copy(castles = c)

  inline def withLastMove(inline m: Uci) = copy(lastMove = Option(m))

  def withCheck(color: Color, v: Boolean) =
    if (v) copy(checkCount = checkCount add color) else this

  def withCheckCount(cc: CheckCount) = copy(checkCount = cc)

  override def toString =
    val positions = (positionHashes.value grouped Hash.size).toList
    s"${lastMove.fold("-")(_.uci)} ${PositionHash.from(positions).map(Hash.debug).mkString(" ")}"

object History:

  def make(
      lastMove: Option[String], // a2a4
      castles: String
  ): History =
    History(
      lastMove = lastMove flatMap Uci.apply,
      castles = Castles(castles),
      positionHashes = Monoid[PositionHash].empty
    )

  def castle(color: Color, kingSide: Boolean, queenSide: Boolean) =
    History(castles = Castles.corners.update(color, kingSide, queenSide))

  def noCastle = History(castles = Castles.none)
