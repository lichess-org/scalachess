package chess

import cats.Eq
import cats.derived.*

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

  private val allByFen: Map[Char, Piece] =
    Map(
      'P' -> Piece(White, Pawn),
      'N' -> Piece(White, Knight),
      'B' -> Piece(White, Bishop),
      'R' -> Piece(White, Rook),
      'Q' -> Piece(White, Queen),
      'K' -> Piece(White, King),
      'p' -> Piece(Black, Pawn),
      'n' -> Piece(Black, Knight),
      'b' -> Piece(Black, Bishop),
      'r' -> Piece(Black, Rook),
      'q' -> Piece(Black, Queen),
      'k' -> Piece(Black, King)
    )

  def fromChar(c: Char): Option[Piece] =
    allByFen.get(c)
