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
  // E4, D4, E5, D5
  val center = 0x1818000000L

  val firstRank: Bitboard = 0xffL
  val lastRank: Bitboard  = 0xffL << 56

  // all light squares
  val lightSquares: Bitboard = 0x55aa55aa55aa55aaL
  // all dark squares
  val darkSquares: Bitboard = 0xaa55aa55aa55aa55L

  inline def file(inline f: File): Bitboard                        = FILES(f.value)
  inline def rank(inline r: Rank): Bitboard                        = RANKS(r.value)
  inline def ray(inline from: Square, inline to: Square): Bitboard = RAYS(from.value)(to.value)

  def aligned(a: Square, b: Square, c: Square): Boolean = ray(a, b).contains(c)
  def between(a: Square, b: Square): Bitboard           = BETWEEN(a.value)(b.value)

  extension (s: Square)

    def bishopAttacks(occupied: Bitboard): Bitboard =
      ATTACKS(Magic.BISHOP(s.value).bitshopIndex(occupied))

    def rookAttacks(occupied: Bitboard): Bitboard =
      ATTACKS(Magic.ROOK(s.value).rookIndex(occupied))

    def queenAttacks(occupied: Bitboard): Bitboard =
      bishopAttacks(occupied) ^ rookAttacks(occupied)

    def pawnAttacks(color: Color): Bitboard =
      color.fold(WHITE_PAWN_ATTACKS(s.value), BLACK_PAWN_ATTACKS(s.value))

    def kingAttacks: Bitboard = KING_ATTACKS(s.value)

    def knightAttacks: Bitboard = KNIGHT_ATTACKS(s.value)

  extension (l: Long)
    private def lsb: Square = Square.unsafe(java.lang.Long.numberOfTrailingZeros(l))
    private def msb: Square = Square.unsafe(63 - java.lang.Long.numberOfLeadingZeros(l))

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

    def isEmpty: Boolean  = a == empty
    def nonEmpty: Boolean = !isEmpty

    def contains(square: Square): Boolean =
      (a & (1L << square.value)) != 0L

    def add(square: Square): Bitboard    = a | square.bl
    def remove(square: Square): Bitboard = a & ~square.bl

    def move(from: Square, to: Square): Bitboard =
      a & ~from.bl | to.bl

    def moreThanOne: Boolean =
      (a & (a - 1L)) != 0L

    // Gets the only square in the set, if there is exactly one.
    def singleSquare: Option[Square] =
      if moreThanOne then None
      else first

    // total non empty squares
    def count: Int = java.lang.Long.bitCount(a)

    // the first non empty square (the least significant bit/ the rightmost bit)
    def first: Option[Square] = Square(java.lang.Long.numberOfTrailingZeros(a))

    // the last non empty square (the most significant bit / the leftmost bit)
    def last: Option[Square] = Square(63 - java.lang.Long.numberOfLeadingZeros(a))

    // remove the first/smallest non empty square
    def removeFirst: Bitboard = a & (a - 1L)

    // remove the last/largest non empty square
    def removeLast: Bitboard = a & ~a.msb.bl

    def isolateFirst: Bitboard = Bitboard(a & -a)

    def isolateLast: Bitboard = last.fold(empty)(_.bl)

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

    // return list of square that sorted ascendingly
    def squares: List[Square] =
      var b       = a
      val builder = List.newBuilder[Square]
      while b != 0L
      do
        builder += b.lsb
        b &= (b - 1L)
      builder.result

    // min square in the bitboard if it is not empty
    def first[B](f: Square => Option[B]): Option[B] =
      var b                 = a
      var result: Option[B] = None
      while b != 0L && result.isEmpty
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    // max square in the bitboard if it is not empty
    def last[B](f: Square => Option[B]): Option[B] =
      var b                 = a
      var result: Option[B] = None
      while b != 0L && result.isEmpty
      do
        result = f(b.msb)
        b &= ~b.msb.bl
      result

    def find(f: Square => Boolean): Option[Square] =
      var b                      = a
      var result: Option[Square] = None
      while b != 0L && result.isEmpty
      do
        if f(b.lsb) then result = Some(b.lsb)
        b &= (b - 1L)
      result

    def findLast(f: Square => Boolean): Option[Square] =
      var b                      = a
      var result: Option[Square] = None
      while b != 0L && result.isEmpty
      do
        if f(b.msb) then result = Some(b.msb)
        b &= ~b.msb.bl
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

    def iterator: Iterator[Square] = new:
      private var b                        = a
      override inline def hasNext: Boolean = b != 0L
      override inline def next: Square =
        val result = b.lsb
        b &= (b - 1L)
        result

    // TODO: nice to have, faster.
    // but should only be used for debug
    // TODO: override toString?
    def display: String =
      val builder = StringBuilder()
      Rank.allReversed.foreach: r =>
        File.all.foreach: f =>
          val s = Square(f, r)
          builder ++= (if contains(s) then "1" else ".")
          if f != File.H then builder ++= " "
          else if s != Square.H1 then builder ++= "\n"
      builder.result
