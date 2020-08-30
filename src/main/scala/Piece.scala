package chess

case class Piece(color: Color, role: Role) {

  def is(c: Color)   = c == color
  def is(r: Role)    = r == role
  def isNot(r: Role) = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean =
    role match {
      case King   => PosSet.kingAttacks(from).has(to)
      case Queen  => PosSet.queenAttacks(from, PosSet.empty).has(to)
      case Rook   => PosSet.rookAttacks(from, PosSet.empty).has(to)
      case Bishop => PosSet.bishopAttacks(from, PosSet.empty).has(to)
      case Knight => PosSet.knightAttacks(from).has(to)
      case Pawn   => Piece.pawnEyes(color, from, to)
    }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    if (role == Pawn) Piece.pawnEyes(color, from, to) || {
      (from ?| to) && {
        val dy = to.rank - from.rank
        if (color.white) (dy == 1 || (from.rank <= Rank.Second && dy == 2))
        else (dy == -1 || (from.rank >= Rank.Seventh && dy == -2))
      }
    }
    else eyes(from, to)

  override def toString = s"$color-$role".toLowerCase
}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Color.fromWhite(c.isUpper), _)
    }

  private def pawnEyes(color: Color, from: Pos, to: Pos) =
    (from xDist to) == 1 && (to.rank - from.rank) == {
      if (color.white) 1 else -1
    }
}
