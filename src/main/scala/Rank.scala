package chess

opaque type Rank = Int
object Rank:
  extension (a: Rank)

    inline def value: Int = a
    inline def index: Int = a

    inline infix def >(inline o: Rank): Boolean  = a > o
    inline infix def <(inline o: Rank): Boolean  = a < o
    inline infix def >=(inline o: Rank): Boolean = a >= o
    inline infix def <=(inline o: Rank): Boolean = a <= o

    inline def offset(delta: Int): Option[Rank] =
      if -8 < delta && delta < 8 then atIndex(a + delta)
      else None

    inline def char: Char = (49 + a).toChar
  end extension

  private[chess] def unsafe(value: Int): Rank = value
  inline def atIndex(index: Int): Option[Rank] =
    if 0 <= index && index < 8 then Some(index)
    else None

  inline def of(inline square: Square): Rank = square.value >> 3

  inline def fromChar(inline ch: Char): Option[Rank] = atIndex(ch.toInt - 49)

  val First   = Rank.unsafe(0)
  val Second  = Rank.unsafe(1)
  val Third   = Rank.unsafe(2)
  val Fourth  = Rank.unsafe(3)
  val Fifth   = Rank.unsafe(4)
  val Sixth   = Rank.unsafe(5)
  val Seventh = Rank.unsafe(6)
  val Eighth  = Rank.unsafe(7)

  val all         = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth)
  val allReversed = all.reverse
