package chess
package bitboard

type BitboardRuntime[A] = SameRuntime[A, Long]
trait OpaqueBitboard[A](using A =:= Long) extends TotalWrapper[A, Long]:

  val empty: A             = 0L.bb
  protected val ALL: A     = -1L.bb
  protected val CORNERS: A = 0x8100000000000081L.bb

  extension (l: Long)
    def bb: A                    = l.asInstanceOf[A]
    private def lsb: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(l))

  extension (s: Pos) inline def bitboard: A = (1L << s.value).bb

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

    def moreThanOne: Boolean =
      (a.value & (a.value - 1L)) != 0L

    def occupiedSquares: List[Pos] =
      fold(List.empty)((xs, pos) => xs :+ pos)

    // total non empty position
    def count: Int = java.lang.Long.bitCount(a)

    // the first non empty position
    def first: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(a))

    // remove the first non empty position
    def removeFirst: A = (a.value & (a.value - 1L)).bb

    def fold[B](init: B)(f: (B, Pos) => B): B =
      var b      = a.value
      var result = init
      while b != 0
      do
        result = f(result, b.lsb.get)
        b &= (b - 1L)
      result

    def flatMap[B](f: Pos => IterableOnce[B]): List[B] =
      var b      = a.value
      var builder = List.newBuilder[B]
      while b != 0
      do
        builder ++= f(b.lsb.get) 
        b &= (b - 1L)
      builder.result

    def isEmpty: Boolean  = a == empty
    def nonEmpty: Boolean = !isEmpty
