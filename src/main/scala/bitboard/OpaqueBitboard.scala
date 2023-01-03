package chess
package bitboard

object OpaqueBitboard:

  type BitboardRuntime[A] = SameRuntime[A, Long]
  trait OpaqueBitboard[A](using A =:= Long) extends TotalWrapper[A, Long]:

    def empty: A   = 0L.bb
    def all: A     = -1L.bb
    def corners: A = 0x8100000000000081L.bb

    extension (l: Long)
      def bb: A = l.asInstanceOf[A]
      def lsb: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(l))


    extension (s: Pos) inline def bitboard: A = (1L << s.value).bb

    extension (a: A)
      inline def unary_- : A                                                     = (-a.value).bb
      inline def unary_~ : A                                                     = (~a.value).bb
      inline infix def >(inline o: Long): Boolean                                = a.value > o
      inline infix def <(inline o: Long): Boolean                                = a.value < o
      inline infix def >=(inline o: Long): Boolean                               = a.value >= o
      inline infix def <=(inline o: Long): Boolean                               = a.value <= o
      inline infix def +(inline o: Long): A                                      = (a.value + o).bb
      inline infix def -(inline o: Long): A                                      = (a.value - o).bb
      inline infix def &(inline o: Long): A                                      = (a.value & o).bb
      inline infix def ^(inline o: Long): A                                      = (a.value ^ o).bb
      inline infix def |(inline o: Long): A                                      = (a.value | o).bb
      inline infix def <<(inline o: Long): A                                     = (a.value << o).bb
      inline infix def >>>(inline o: Long): A                                    = (a.value >>> o).bb
      inline infix def >[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean  = >(sr(o))
      inline infix def <[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean  = <(sr(o))
      inline infix def >=[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean = >=(sr(o))
      inline infix def <=[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean = <=(sr(o))
      inline infix def +[B](inline o: B)(using sr: BitboardRuntime[B]): A        = a + sr(o)
      inline infix def -[B](inline o: B)(using sr: BitboardRuntime[B]): A        = a - sr(o)
      inline infix def &[B](inline o: B)(using sr: BitboardRuntime[B]): A        = a & sr(o)
      inline infix def ^[B](inline o: B)(using sr: BitboardRuntime[B]): A        = a ^ sr(o)
      inline infix def |[B](inline o: B)(using sr: BitboardRuntime[B]): A        = a | sr(o)
      inline infix def <<[B](inline o: B)(using sr: BitboardRuntime[B]): A       = a << sr(o)
      inline infix def >>>[B](inline o: B)(using sr: BitboardRuntime[B]): A      = a >>> sr(o)

      def contains(pos: Pos): Boolean =
        (a.value & (1L << pos.value)) != 0L

      def moreThanOne: Boolean =
        (a.value & (a.value - 1L)) != 0L

      def occupiedSquares: List[Pos] =
        fold(List[Pos]())((xs, pos) => xs :+ pos)

      // total non empty position
      def count: Int =
        fold(0)((count, _) => count + 1)

      def lsb: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(a))

      def fold[A](init: A)(f: (A, Pos) => A): A =
        var b      = a.value
        var result = init
        while b != 0
        do
          result = f(result, b.lsb.get)
          b &= (b - 1L)
        result

      def isEmpty: Boolean  = a == empty
      def nonEmpty: Boolean = !isEmpty
