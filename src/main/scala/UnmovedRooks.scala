package chess

import bitboard.OpaqueBitboard
import bitboard.Bitboard

opaque type UnmovedRooks = Long
object UnmovedRooks extends OpaqueBitboard[UnmovedRooks]:
  // for lila testing only
  val default: UnmovedRooks = UnmovedRooks(Bitboard.rank(Rank.First) | Bitboard.rank(Rank.Eighth))
  val corners: UnmovedRooks = CORNERS
  val none: UnmovedRooks    = empty

  def apply(b: Bitboard): UnmovedRooks       = b.value
  def apply(xs: Iterable[Pos]): UnmovedRooks = Bitboard(xs).value

  extension (ur: UnmovedRooks)
    def toList: List[Pos] = ur.occupiedSquares

    def without(color: Color): UnmovedRooks =
      ur & Bitboard.rank(color.lastRank)

    // Try to guess the side of the rook at postion `pos`
    // If the position is not a ummovedRook return None
    // If the position is a ummovedRook but there is no other rook on the
    // same rank return Some(None) (because we cannot guess)
    // If there are two rooks on the same rank, return the side of the rook
    def side(pos: Pos): Option[Option[Side]] =
      val rook = pos.bb
      if ur.sharedAny(rook) then None
      else
        (ur & ~rook & Bitboard.rank(pos.rank)).first match
          case Some(otherRook) =>
            if (otherRook.file > pos.file) then Some(Some(QueenSide))
            else Some(Some(KingSide))
          case None => Some(None)
