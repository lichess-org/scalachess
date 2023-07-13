package chess

import cats.Eq
import cats.derived.*

import bitboard.Bitboard
import bitboard.Bitboard.*

case class Piece(color: Color, role: Role) derives Eq:

  def is(c: Color)   = c == color
  def is(r: Role)    = r == role
  def isNot(r: Role) = r != role
  def unary_!        = Piece(!color, role)

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if color.white then role.forsythUpper else role.forsyth

  // the piece at from can attack the target to when mask are all the occupied squares
  def eyes(from: Square, to: Square, mask: Bitboard): Boolean =
    role match
      case King   => from.kingAttacks.contains(to)
      case Queen  => from.queenAttacks(mask).contains(to)
      case Rook   => from.rookAttacks(mask).contains(to)
      case Bishop => from.bishopAttacks(mask).contains(to)
      case Knight => from.knightAttacks.contains(to)
      case Pawn   => from.pawnAttacks(color).contains(to)

  override def toString = s"$color-$role".toLowerCase

object Piece:

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map:
      Piece(Color.fromWhite(c.isUpper), _)
