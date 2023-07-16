package chess
package bitboard

import scala.annotation.targetName

opaque type Bitboard = Long
object Bitboard:
  import Attacks.*

  def apply(l: Long): Bitboard                            = l
  inline def apply(inline xs: Iterable[Square]): Bitboard = xs.foldLeft(empty)((b, s) => b | s.bl)

  val empty: Bitboard = 0L
  val all: Bitboard   = -1L

  val firstRank: Bitboard = 0xffL
  val lastRank: Bitboard  = 0xffL << 56

  // all light squares
  val lightSquares: Bitboard = 0x55aa55aa55aa55aaL
  // all dark squares
  val darkSquares: Bitboard = 0xaa55aa55aa55aa55L

  inline def file(inline f: File): Bitboard                        = FILES(f.value)
  inline def ray(inline from: Square, inline to: Square): Bitboard = RAYS(from.value)(to.value)
  inline def rank(inline r: Rank): Bitboard                        = RANKS(r.value)

  def aligned(a: Square, b: Square, c: Square): Boolean =
    ray(a, b).contains(c)

  def between(a: Square, b: Square): Bitboard =
    BETWEEN(a.value)(b.value)

  extension (s: Square)

    def bishopAttacks(occupied: Bitboard): Bitboard =
      val magic = Magic.BISHOP(s.value)
      ATTACKS(magic.bitshopIndex(occupied))

    def rookAttacks(occupied: Bitboard): Bitboard =
      val magic = Magic.ROOK(s.value)
      ATTACKS(magic.rookIndex(occupied))

    def queenAttacks(occupied: Bitboard): Bitboard =
      bishopAttacks(occupied) ^ rookAttacks(occupied)

    def pawnAttacks(color: Color): Bitboard =
      color.fold(WHITE_PAWN_ATTACKS(s.value), BLACK_PAWN_ATTACKS(s.value))

    def kingAttacks: Bitboard =
      KING_ATTACKS(s.value)

    def knightAttacks: Bitboard =
      KNIGHT_ATTACKS(s.value)

  extension (l: Long) private def lsb: Square = Square(java.lang.Long.numberOfTrailingZeros(l))

  extension (a: Bitboard)
    inline def value: Long                        = a
    inline def unary_~ : Bitboard                 = (~a)
    inline infix def &(inline o: Long): Bitboard  = (a & o)
    inline infix def ^(inline o: Long): Bitboard  = (a ^ o)
    inline infix def |(inline o: Long): Bitboard  = (a | o)
    inline infix def <<(inline o: Int): Bitboard  = (a << o)
    inline infix def >>>(inline o: Int): Bitboard = (a >>> o)
    @targetName("and")
    inline infix def &(o: Bitboard): Bitboard = (a & o)
    @targetName("xor")
    inline infix def ^(o: Bitboard): Bitboard = (a ^ o)
    @targetName("or")
    inline infix def |(o: Bitboard): Bitboard = (a | o)

    def contains(square: Square): Boolean =
      (a & (1L << square.value)) != 0L

    def addSquare(square: Square): Bitboard    = a | square.bb
    def removeSquare(square: Square): Bitboard = a & ~square.bb

    def move(from: Square, to: Square): Bitboard =
      a & ~from.bb | to.bb

    def moreThanOne: Boolean =
      (a & (a - 1L)) != 0L

    // Gets the only square in the set, if there is exactly one.
    def singleSquare: Option[Square] =
      if moreThanOne then None
      else first

    def squares: List[Square] =
      var b       = a
      val builder = List.newBuilder[Square]
      while b != 0L
      do
        builder += b.lsb
        b &= (b - 1L)
      builder.result

    // total non empty squares
    def count: Int = java.lang.Long.bitCount(a)

    // the first non empty square (the least significant bit/ the rightmost bit)
    def first: Option[Square] = Square.at(java.lang.Long.numberOfTrailingZeros(a))

    // the last non empty square (the most significant bit / the leftmost bit)
    def last: Option[Square] = Square.at(63 - java.lang.Long.numberOfLeadingZeros(a))

    // remove the first non empty position
    def removeFirst: Bitboard = a & (a - 1L)

    inline def intersects(inline o: Long): Boolean =
      (a & o) != 0L

    @targetName("intersectsB")
    inline def intersects(o: Bitboard): Boolean =
      (a & o).nonEmpty

    inline def isDisjoint(inline o: Long): Boolean =
      (a & o).isEmpty

    @targetName("isDisjointB")
    inline def isDisjoint(o: Bitboard): Boolean =
      (a & o).isEmpty

    def first[B](f: Square => Option[B]): Option[B] =
      var b                 = a
      var result: Option[B] = None
      while b != 0L && result.isEmpty
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def fold[B](init: B)(f: (B, Square) => B): B =
      var b      = a
      var result = init
      while b != 0L
      do
        result = f(result, b.lsb)
        b &= (b - 1L)
      result

    def filter(f: Square => Boolean): List[Square] =
      val builder = List.newBuilder[Square]
      var b       = a
      while b != 0L
      do
        if f(b.lsb) then builder += b.lsb
        b &= (b - 1L)
      builder.result

    def withFilter(f: Square => Boolean): List[Square] =
      filter(f)

    def foreach[U](f: Square => U): Unit =
      var b = a
      while b != 0L
      do
        f(b.lsb)
        b &= (b - 1L)

    def forall[B](f: Square => Boolean): Boolean =
      var b      = a
      var result = true
      while b != 0L && result
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def exists[B](f: Square => Boolean): Boolean =
      var b      = a
      var result = false
      while b != 0L && !result
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def flatMap[B](f: Square => IterableOnce[B]): List[B] =
      var b       = a
      val builder = List.newBuilder[B]
      while b != 0L
      do
        builder ++= f(b.lsb)
        b &= (b - 1L)
      builder.result

    def map[B](f: Square => B): List[B] =
      var b       = a
      val builder = List.newBuilder[B]
      while b != 0L
      do
        builder += f(b.lsb)
        b &= (b - 1L)
      builder.result

    def isEmpty: Boolean  = a == empty
    def nonEmpty: Boolean = !isEmpty
