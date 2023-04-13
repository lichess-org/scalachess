package chess

import bitboard.OpaqueBitboard
import bitboard.Bitboard

opaque type UnmovedRooks = Long
object UnmovedRooks extends OpaqueBitboard[UnmovedRooks]:
  // for lila testing only
  val default: UnmovedRooks = UnmovedRooks(Bitboard.rank(Rank.First) | Bitboard.rank(Rank.Eighth))
  val corners: UnmovedRooks = CORNERS
  val none: UnmovedRooks    = empty

  def apply(b: Bitboard): UnmovedRooks = b.value

  // guess unmovedRooks from board
  // we assume rooks are on their initial position
  def from(board: Board): UnmovedRooks =
    val wr = board.rooks & board.white & Bitboard.rank(White.backRank)
    val br = board.rooks & board.black & Bitboard.rank(Black.backRank)
    UnmovedRooks(wr | br)

  extension (ur: UnmovedRooks)
    def toList: List[Square] = ur.squares

    def without(color: Color): UnmovedRooks =
      ur & Bitboard.rank(color.lastRank)

    // Try to guess the side of the rook at postion `square`
    // If the position is not a ummovedRook return None
    // If the position is a ummovedRook but there is no other rook on the
    // same rank return Some(None) (because we cannot guess)
    // If there are two rooks on the same rank, return the side of the rook
    def side(square: Square): Option[Option[Side]] =
      val rook = square.bb
      if ur.isDisjoint(rook) then None
      else
        (ur & ~rook & Bitboard.rank(square.rank)).first match
          case Some(otherRook) =>
            if (otherRook.file > square.file) then Some(Some(QueenSide))
            else Some(Some(KingSide))
          case None => Some(None)
