package chess

import bitboard.Bitboard
import scala.annotation.targetName

opaque type UnmovedRooks = Long
object UnmovedRooks:
  // for lila testing only
  val default: UnmovedRooks = UnmovedRooks(Bitboard.rank(Rank.First) | Bitboard.rank(Rank.Eighth))
  val corners: UnmovedRooks = 0x8100000000000081L
  val none: UnmovedRooks    = 0L

  @targetName("applyUnmovedRooks")
  def apply(b: Bitboard): UnmovedRooks                        = b.value
  def apply(l: Long): UnmovedRooks                            = l
  inline def apply(inline xs: Iterable[Square]): UnmovedRooks = xs.foldLeft(none)((b, s) => b | s.bl)

  // guess unmovedRooks from board
  // we assume rooks are on their initial position
  def from(board: Board): UnmovedRooks =
    val wr = board.rooks & board.white & Bitboard.rank(White.backRank)
    val br = board.rooks & board.black & Bitboard.rank(Black.backRank)
    UnmovedRooks(wr | br)

  extension (ur: UnmovedRooks)
    inline def bb: Bitboard  = Bitboard(ur)
    def value: Long          = ur
    def toList: List[Square] = ur.bb.squares

    def without(color: Color): UnmovedRooks =
      ur & Bitboard.rank(color.lastRank).value

    // Try to guess the side of the rook at postion `square`
    // If the position is not a ummovedRook return None
    // If the position is a ummovedRook but there is no other rook on the
    // same rank return Some(None) (because we cannot guess)
    // If there are two rooks on the same rank, return the side of the rook
    def side(square: Square): Option[Option[Side]] =
      val rook = square.bb
      if rook.isDisjoint(ur) then None
      else
        (Bitboard.rank(square.rank) & ~rook & ur.value).first match
          case Some(otherRook) =>
            if otherRook.file > square.file then Some(Some(QueenSide))
            else Some(Some(KingSide))
          case None => Some(None)

    def contains(square: Square): Boolean =
      (ur & (1L << square.value)) != 0L

    inline def unary_~ : UnmovedRooks                = ~ur
    inline infix def &(inline o: Long): UnmovedRooks = ur & o
    inline infix def ^(inline o: Long): UnmovedRooks = ur ^ o
    inline infix def |(inline o: Long): UnmovedRooks = ur | o

    @targetName("and")
    inline infix def &(o: Bitboard): UnmovedRooks = ur & o.value
    @targetName("xor")
    inline infix def ^(o: Bitboard): UnmovedRooks = ur ^ o.value
    @targetName("or")
    inline infix def |(o: Bitboard): UnmovedRooks = ur | o.value
