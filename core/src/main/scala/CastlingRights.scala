package chess

import scala.annotation.targetName

/** Castling rights as a bitboard of rook squares that may still participate in castling.
  *
  * A bit set at square `s` means: the rook currently sitting on `s` has neither moved
  * nor been captured since the game started, and its king on the same back rank has not
  * moved either — so castling using this rook is still permitted (subject to the usual
  * runtime checks: clear path, king not in/through/into check).
  *
  * The bit is cleared when:
  *   - the rook on `s` moves (it loses its right),
  *   - the rook on `s` is captured,
  *   - the king of the same color moves (or exploded) (all bits on that color's back rank are cleared).
  */
opaque type CastlingRights = Long
object CastlingRights:

  /** No castling rights for either side. */
  val none: CastlingRights = 0L

  /** The four corner squares A1, H1, A8, H8 — the standard-chess starting rights. */
  val corners: CastlingRights = 0x8100000000000081L

  /** Standard-chess initial castling rights (alias for [[corners]]). */
  val init: CastlingRights = corners

  /** Only black has castling rights at the corners A8 and H8 — used by Horde. */
  val black: CastlingRights = 0x8100000000000000L

  @targetName("applyCastlingRights")
  def apply(b: Bitboard): CastlingRights = b.value
  def apply(l: Long): CastlingRights = l
  inline def apply(inline xs: Iterable[Square]): CastlingRights =
    xs.foldLeft(none)((b, s) => b | s.bl)

  def apply(
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ): CastlingRights =
    (if whiteKingSide then Square.H1.bl else 0L) |
      (if whiteQueenSide then Square.A1.bl else 0L) |
      (if blackKingSide then Square.H8.bl else 0L) |
      (if blackQueenSide then Square.A8.bl else 0L)

  // guess castling rights from board (assumes rooks are on their initial position)
  def from(board: Board): CastlingRights =
    val wr = board.rooks & board.white & Bitboard.rank(White.backRank)
    val br = board.rooks & board.black & Bitboard.rank(Black.backRank)
    CastlingRights(wr | br)

  extension (cr: CastlingRights)
    inline def bb: Bitboard = Bitboard(cr)
    inline def value: Long = cr
    def isEmpty: Boolean = cr == 0L
    def nonEmpty: Boolean = cr != 0L
    def toList: List[Square] = cr.bb.squares

    def canCastle(kings: Bitboard, side: Side): Boolean =
      if cr == 0L || kings.isEmpty then false
      else
        // unsafe but we know kings is nonEmpty
        val king = Square.unsafe(java.lang.Long.numberOfTrailingZeros(kings.value))
        // restrict to rooks on the king's rank, so the other color's rights don't leak in
        val rights = cr & king.rank.bb.value
        side match
          case Side.KingSide => (rights & king.file.above).nonEmpty
          case Side.QueenSide => (rights & king.file.below).nonEmpty

    /** King-side / queen-side castling availability for `kings` in a single pass.
      *
      * Equivalent to `(canCastle(kings, KingSide), canCastle(kings, QueenSide))` but
      * shares the king-square and back-rank-mask work — used by the position hash, which
      * needs both bits per color on every move.
      */
    inline def foldCastle[A](kings: Bitboard)(inline f: (kingSide: Boolean, queenSide: Boolean) => A): A =
      if cr == 0L || kings.isEmpty then f(false, false)
      else
        val king = Square.unsafe(java.lang.Long.numberOfTrailingZeros(kings.value))
        val rights = cr & king.rank.bb.value
        f((rights & king.file.above).nonEmpty, (rights & king.file.below).nonEmpty)

    def without(color: Color): CastlingRights =
      cr & ~Bitboard.rank(color.backRank).value

    def contains(square: Square): Boolean =
      (cr & (1L << square.value)) != 0L

    inline def unary_~ : CastlingRights = ~cr
    inline infix def &(inline o: Long): CastlingRights = cr & o
    inline infix def ^(inline o: Long): CastlingRights = cr ^ o
    inline infix def |(inline o: Long): CastlingRights = cr | o

    @targetName("and")
    inline infix def &(o: Bitboard): CastlingRights = cr & o.value
    @targetName("xor")
    inline infix def ^(o: Bitboard): CastlingRights = cr ^ o.value
    @targetName("or")
    inline infix def |(o: Bitboard): CastlingRights = cr | o.value
