package chess

import format.Uci
import cats.kernel.Monoid

// Checks received by the respective side.
case class CheckCount(white: Int = 0, black: Int = 0):

  def add(color: Color) =
    copy(
      white = white + color.fold(1, 0),
      black = black + color.fold(0, 1)
    )

  def nonEmpty = white > 0 || black > 0

  def apply(color: Color) = color.fold(white, black)

opaque type UnmovedRooks = Set[Pos]
object UnmovedRooks extends TotalWrapper[UnmovedRooks, Set[Pos]]:
  val default: UnmovedRooks = (Pos.whiteBackrank ::: Pos.blackBackrank).toSet

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Monoid[PositionHash].empty,
    castles: Castles = Castles.all,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks = UnmovedRooks.default,
    halfMoveClock: HalfMoveClock = HalfMoveClock(0)
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
