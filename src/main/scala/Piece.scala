package chess

case class Piece(color: Color, role: Role):

  def is(c: Color)   = c == color
  def is(r: Role)    = r == role
  def isNot(r: Role) = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if color.white then role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  import bitboard.Bitboard
  import bitboard.Bitboard.*
  def eyes(from: Pos, to: Pos): Boolean =
    val occupied: Bitboard = to.bb
    role match
      case King   => from.kingAttacks.contains(to)
      case Queen  => from.queenAttacks(occupied).nonEmpty
      case Rook   => from.rookAttacks(occupied).nonEmpty
      case Bishop => from.bishopAttacks(occupied).nonEmpty
      case Knight => from.knightAttacks.contains(to)
      case Pawn   => from.pawnAttacks(color).contains(to)

  override def toString = s"$color-$role".toLowerCase

object Piece:

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Color.fromWhite(c.isUpper), _)
    }
