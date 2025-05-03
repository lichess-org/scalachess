package chess

import chess.variant.Crazyhouse

import format.Uci

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = PositionHash.empty,
    castles: Castles = Castles.init,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks,
    halfMoveClock: HalfMoveClock = HalfMoveClock.initial,
    crazyData: Option[Crazyhouse.Data]
):

  def setHalfMoveClock(v: HalfMoveClock): History = copy(halfMoveClock = v)

  inline def threefoldRepetition: Boolean = positionHashes.isRepetition(3)
  inline def fivefoldRepetition: Boolean  = positionHashes.isRepetition(5)

  inline def canCastle(inline color: Color): Boolean                    = castles.can(color)
  inline def canCastle(inline color: Color, inline side: Side): Boolean = castles.can(color, side)

  inline def withoutCastles(inline color: Color): History = copy(castles = castles.without(color))

  inline def withoutAnyCastles: History = copy(castles = Castles.none)

  inline def withoutCastle(color: Color, side: Side): History = copy(castles = castles.without(color, side))

  inline def withCastles(inline c: Castles): History = copy(castles = c)

  inline def withLastMove(inline m: Uci): History = copy(lastMove = Option(m))

  def withCheck(color: Color, check: Check): History =
    if check.yes then copy(checkCount = checkCount.add(color)) else this

  def withCheckCount(cc: CheckCount): History = copy(checkCount = cc)

// Checks received by the respective side.
case class CheckCount(white: Int = 0, black: Int = 0):

  def add(color: Color): CheckCount =
    copy(
      white = white + color.fold(1, 0),
      black = black + color.fold(0, 1)
    )

  def apply(color: Color): Int = color.fold(white, black)

  def nonEmpty: Boolean = white > 0 || black > 0
