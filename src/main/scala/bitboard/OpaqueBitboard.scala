package chess
package bitboard

type BitboardRuntime[A] = SameRuntime[A, Long]
trait OpaqueBitboard[A](using A =:= Long) extends TotalWrapper[A, Long]:

  val empty: A             = 0L.bb
  protected val ALL: A     = -1L.bb
  protected val CORNERS: A = 0x8100000000000081L.bb

  inline def apply(inline xs: Iterable[Square]): A = xs.foldLeft(empty)((b, p) => b | p.bb)

  extension (l: Long)
    def bb: A               = l.asInstanceOf[A]
    private def lsb: Square = Square(java.lang.Long.numberOfTrailingZeros(l))

  extension (s: Square) inline def bb: A = (1L << s.value).bb

  extension (a: A)
    inline def unary_~ : A                                                = (~a.value).bb
    inline infix def &(inline o: Long): A                                 = (a.value & o).bb
    inline infix def ^(inline o: Long): A                                 = (a.value ^ o).bb
    inline infix def |(inline o: Long): A                                 = (a.value | o).bb
    inline infix def <<(inline o: Long): A                                = (a.value << o).bb
    inline infix def >>>(inline o: Long): A                               = (a.value >>> o).bb
    inline infix def &[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a & sr(o)
    inline infix def ^[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a ^ sr(o)
    inline infix def |[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a | sr(o)
    inline infix def <<[B](inline o: B)(using sr: BitboardRuntime[B]): A  = a << sr(o)
    inline infix def >>>[B](inline o: B)(using sr: BitboardRuntime[B]): A = a >>> sr(o)

    def contains(square: Square): Boolean =
      (a.value & (1L << square.value)) != 0L

    def addSquare(square: Square): A    = a | square.bb
    def removeSquare(square: Square): A = a & ~square.bb

    def move(from: Square, to: Square): A =
      a & ~from.bb | to.bb

    def moreThanOne: Boolean =
      (a.value & (a.value - 1L)) != 0L

    // Gets the only square in the set, if there is exactly one.
    def singleSquare: Option[Square] =
      if moreThanOne then None
      else first

    def squares: List[Square] =
      var b       = a.value
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
    def removeFirst: A = (a.value & (a.value - 1L)).bb

    inline def intersects(inline o: Long): Boolean =
      (a.value & o) != 0L

    inline def intersects[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean =
      (a & sr(o)).nonEmpty

    inline def isDisjoint(inline o: Long): Boolean =
      (a & o).isEmpty

    inline def isDisjoint[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean =
      (a & sr(o)).isEmpty

    def first[B](f: Square => Option[B]): Option[B] =
      var b                 = a.value
      var result: Option[B] = None
      while b != 0L && result.isEmpty
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def fold[B](init: B)(f: (B, Square) => B): B =
      var b      = a.value
      var result = init
      while b != 0L
      do
        result = f(result, b.lsb)
        b &= (b - 1L)
      result

    def filter(f: Square => Boolean): List[Square] =
      val builder = List.newBuilder[Square]
      var b       = a.value
      while b != 0L
      do
        if f(b.lsb) then builder += b.lsb
        b &= (b - 1L)
      builder.result

    def withFilter(f: Square => Boolean): List[Square] =
      filter(f)

    def foreach[U](f: Square => U): Unit =
      var b = a.value
      while b != 0L
      do
        f(b.lsb)
        b &= (b - 1L)

    def forall[B](f: Square => Boolean): Boolean =
      var b      = a.value
      var result = true
      while b != 0L && result
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def exists[B](f: Square => Boolean): Boolean =
      var b      = a.value
      var result = false
      while b != 0L && !result
      do
        result = f(b.lsb)
        b &= (b - 1L)
      result

    def flatMap[B](f: Square => IterableOnce[B]): List[B] =
      var b       = a.value
      val builder = List.newBuilder[B]
      while b != 0L
      do
        builder ++= f(b.lsb)
        b &= (b - 1L)
      builder.result

    def map[B](f: Square => B): List[B] =
      var b       = a.value
      val builder = List.newBuilder[B]
      while b != 0L
      do
        builder += f(b.lsb)
        b &= (b - 1L)
      builder.result

    def isEmpty: Boolean  = a == empty
    def nonEmpty: Boolean = !isEmpty
