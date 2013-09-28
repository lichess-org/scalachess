package chess

case class Piece(color: Color, role: Role) {

  def is(c: Color) = c == color
  def is(r: Role) = r == role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char = if (color == White) role.forsyth.toUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean = role match {
    case King   ⇒ from touches to
    case Queen  ⇒ (from ?| to) || (from ?- to) || (from onSameDiagonal to)
    case Rook   ⇒ (from ?| to) || (from ?- to)
    case Bishop ⇒ from onSameDiagonal to
    case Knight ⇒ {
      val xd = from xDist to
      val yd = from yDist to
      (xd == 1 && yd == 2) || (xd == 2 && yd == 1)
    }
    case Pawn ⇒ (from xDist to) == 1 && (to.y - from.y) == {
      if (color.white) 1 else -1
    }
  }

  override def toString = (color + "-" + role).toLowerCase
}
