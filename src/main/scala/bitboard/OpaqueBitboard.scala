package chess
package bitboard

type BitboardRuntime[A] = SameRuntime[A, Long]
trait OpaqueBitboard[A](using A =:= Long) extends TotalWrapper[A, Long]:

  val empty: A             = 0L.bb
  protected val ALL: A     = -1L.bb
  protected val CORNERS: A = 0x8100000000000081L.bb

  inline def apply(inline xs: Iterable[Pos]): A = xs.foldLeft(empty)((b, p) => b | p.bb)

  extension (l: Long)
    def bb: A                    = l.asInstanceOf[A]
    private def lsb: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(l))

  extension (s: Pos) inline def bb: A = (1L << s.value).bb

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

    def contains(pos: Pos): Boolean =
      (a.value & (1L << pos.value)) != 0L

    def addPos(pos: Pos): A = a | pos.bb

    def moreThanOne: Boolean =
      (a.value & (a.value - 1L)) != 0L

    // Gets the only square in the set, if there is exactly one.
    def singleSquare: Option[Pos] =
      if moreThanOne then None
      else first

    def squares: List[Pos] =
      fold(List.empty)((xs, pos) => pos :: xs)

    // total non empty position
    def count: Int = java.lang.Long.bitCount(a)

    // the first non empty position
    def first: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(a))

    // remove the first non empty position
    def removeFirst: A = (a.value & (a.value - 1L)).bb

    def intersects(o: Long): Boolean =
      (a.value & o) != 0L

    def intersects[B](o: B)(using sr: BitboardRuntime[B]): Boolean =
      (a & sr(o)).nonEmpty

    def isDisjoint(o: Long): Boolean =
      (a & o).isEmpty

    def isDisjoint[B](o: B)(using sr: BitboardRuntime[B]): Boolean =
      (a & sr(o)).isEmpty

    def first[B](f: Pos => Option[B]): Option[B] =
      var b                 = a.value
      var result: Option[B] = None
      while b != 0L && result.isEmpty
      do
        result = f(b.lsb.get)
        b &= (b - 1L)
      result

    def fold[B](init: B)(f: (B, Pos) => B): B =
      var b      = a.value
      var result = init
      while b != 0L
      do
        result = f(result, b.lsb.get)
        b &= (b - 1L)
      result

    def flatMap[B](f: Pos => IterableOnce[B]): List[B] =
      var b       = a.value
      val builder = List.newBuilder[B]
      while b != 0L
      do
        builder ++= f(b.lsb.get)
        b &= (b - 1L)
      builder.result

    def isEmpty: Boolean  = a == empty
    def nonEmpty: Boolean = !isEmpty
