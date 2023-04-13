package chess

opaque type Rank = Int
object Rank extends OpaqueInt[Rank]:
  extension (a: Rank)

    inline def index: Int = a

    inline def offset(delta: Int): Option[Rank] =
      if (-8 < delta && delta < 8) atIndex(a + delta)
      else None

    inline def char: Char = (49 + a).toChar
  end extension

  inline def atIndex(index: Int): Option[Rank] =
    if (0 <= index && index < 8) Some(index)
    else None

  inline def of(inline square: Square): Rank = square.value >> 3

  inline def fromChar(inline ch: Char): Option[Rank] = atIndex(ch.toInt - 49)

  val First   = Rank(0)
  val Second  = Rank(1)
  val Third   = Rank(2)
  val Fourth  = Rank(3)
  val Fifth   = Rank(4)
  val Sixth   = Rank(5)
  val Seventh = Rank(6)
  val Eighth  = Rank(7)

  val all         = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth)
  val allReversed = all.reverse
