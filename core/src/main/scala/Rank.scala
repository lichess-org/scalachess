package chess

opaque type Rank = Int
object Rank:
  extension (a: Rank)
    inline def value: Int = a

    inline infix def >(inline o: Rank): Boolean = value > o.value
    inline infix def <(inline o: Rank): Boolean = value < o.value
    inline infix def >=(inline o: Rank): Boolean = value >= o.value
    inline infix def <=(inline o: Rank): Boolean = value <= o.value

    inline def char: Char = (49 + a).toChar

    // the bitboard of the rank
    inline def bb: Bitboard = Bitboard.rank(value)
  end extension

  inline def apply(index: Int): Option[Rank] = Option.when(0 <= index && index < 8)(index)

  inline def of(inline square: Square): Rank = square.value >> 3

  inline def fromChar(inline ch: Char): Option[Rank] = Rank(ch.toInt - 49)

  val First: Rank = 0
  val Second: Rank = 1
  val Third: Rank = 2
  val Fourth: Rank = 3
  val Fifth: Rank = 4
  val Sixth: Rank = 5
  val Seventh: Rank = 6
  val Eighth: Rank = 7

  val all = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth)
  val allReversed = all.reverse
