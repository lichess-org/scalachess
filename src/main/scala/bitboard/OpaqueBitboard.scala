package chess
package bitboard

object OpaqueBitboard:

  type BitboardRuntime[A] = SameRuntime[A, Long]
  trait OpaqueBitboard[A](using A =:= Long) extends TotalWrapper[A, Long]:


    def empty: A = 0L.bb
    def all: A = 1L.bb
    def corners: A = 0x8100000000000081L.bb

    extension (l: Long)
      def bb: A = l.asInstanceOf[A]

    extension (s: Pos)
      inline def bitboard: A = (1L << s.value).bb

    extension (a: A)
      inline def unary_- : A                                                   = -a
      inline def unary_~ : A                                                   = ~a
      inline infix def >(inline o: Long): Boolean                              = a > o
      inline infix def <(inline o: Long): Boolean                              = a < o
      inline infix def >=(inline o: Long): Boolean                             = a >= o
      inline infix def <=(inline o: Long): Boolean                             = a <= o
      inline infix def +(inline o: Long): A                             = a + o
      inline infix def -(inline o: Long): A                             = a - o
      inline infix def &(inline o: Long): A                             = a & o
      inline infix def ^(inline o: Long): A                             = a ^ o
      inline infix def |(inline o: Long): A                             = a | o
      inline infix def <<(inline o: Long): A                            = a << o
      inline infix def >>>(inline o: Long): A                           = a >>> o
      inline infix def >[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean    = >(sr(o))
      inline infix def <[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean    = <(sr(o))
      inline infix def >=[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean   = >=(sr(o))
      inline infix def <=[B](inline o: B)(using sr: BitboardRuntime[B]): Boolean   = <=(sr(o))
      inline infix def +[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a + sr(o)
      inline infix def -[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a - sr(o)
      inline infix def &[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a & sr(o)
      inline infix def ^[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a ^ sr(o)
      inline infix def |[B](inline o: B)(using sr: BitboardRuntime[B]): A   = a | sr(o)
      inline infix def <<[B](inline o: B)(using sr: BitboardRuntime[B]): A  = a << sr(o)
      inline infix def >>>[B](inline o: B)(using sr: BitboardRuntime[B]): A = a >>> sr(o)

      def contains(pos: Pos): Boolean =
        (a & (1L << pos.value)).nonEmpty

      def moreThanOne: Boolean =
        (a & (a - 1L)).nonEmpty

      def lsb: Option[Pos] = Pos.at(java.lang.Long.numberOfTrailingZeros(a))

      def occupiedSquares: List[Pos] =
        fold(List[Pos]())((xs, pos) => xs :+ pos)

      // total non empty position
      def count: Int =
        fold(0)((count, _) => count + 1)

      def fold[A](init: A)(f: (A, Pos) => A): A =
        var b     = a
        var result = init
        while b != 0
        do
          result = f(result, b.lsb.get)
          b &= (b - 1L)
        result

      def isEmpty: Boolean  = a == empty
      def nonEmpty: Boolean = !isEmpty
