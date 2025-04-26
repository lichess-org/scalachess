package chess

import chess.variant.Crazyhouse

import format.Uci

// Checks received by the respective side.
case class CheckCount(white: Int = 0, black: Int = 0):

  def add(color: Color) =
    copy(
      white = white + color.fold(1, 0),
      black = black + color.fold(0, 1)
    )

  def nonEmpty = white > 0 || black > 0

  def apply(color: Color) = color.fold(white, black)

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = PositionHash.empty,
    castles: Castles = Castles.init,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks,
    halfMoveClock: HalfMoveClock = HalfMoveClock.initial,
    crazyData: Option[Crazyhouse.Data]
):

  def setHalfMoveClock(v: HalfMoveClock) = copy(halfMoveClock = v)

  inline def threefoldRepetition = positionHashes.isRepetition(3)
  inline def fivefoldRepetition  = positionHashes.isRepetition(5)

  inline def canCastle(inline color: Color)                    = castles.can(color)
  inline def canCastle(inline color: Color, inline side: Side) = castles.can(color, side)

  inline def withoutCastles(inline color: Color) = copy(castles = castles.without(color))

  inline def withoutAnyCastles = copy(castles = Castles.none)

  inline def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  inline def withCastles(inline c: Castles) = copy(castles = c)

  inline def withLastMove(inline m: Uci) = copy(lastMove = Option(m))

  def withCheck(color: Color, check: Check) =
    if check.yes then copy(checkCount = checkCount.add(color)) else this

  def withCheckCount(cc: CheckCount) = copy(checkCount = cc)
