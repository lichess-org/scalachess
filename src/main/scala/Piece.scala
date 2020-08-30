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
  def eyes(from: Pos, to: Pos): Boolean = attacks(from, PosSet.empty).has(to)

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

  def attacks(from: Pos, occupied: => PosSet): PosSet =
    role match {
      case King   => PosSet.kingAttacks(from)
      case Queen  => PosSet.queenAttacks(from, occupied)
      case Rook   => PosSet.rookAttacks(from, occupied)
      case Bishop => PosSet.bishopAttacks(from, occupied)
      case Knight => PosSet.knightAttacks(from)
      case Pawn   => PosSet.pawnAttacks(color, from)
    }

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
