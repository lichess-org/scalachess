package chess

import chess.variant.Crazyhouse

import format.Uci

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = PositionHash.empty,
    checkCount: CheckCount = CheckCount(0, 0),
    castlingRights: CastlingRights,
    halfMoveClock: HalfMoveClock = HalfMoveClock.initial,
    crazyData: Option[Crazyhouse.Data]
):

  def setHalfMoveClock(v: HalfMoveClock): History = copy(halfMoveClock = v)

  inline def threefoldRepetition: Boolean = positionHashes.isRepetition(3)
  inline def fivefoldRepetition: Boolean = positionHashes.isRepetition(5)

  inline def withoutCastlingRights(inline color: Color): History =
    copy(castlingRights = castlingRights.without(color))

  inline def withoutAnyCastlingRights: History = copy(castlingRights = CastlingRights.none)

  inline def withCastlingRights(inline cr: CastlingRights): History = copy(castlingRights = cr)

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
