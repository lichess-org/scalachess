package chess

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    if (-8 < delta && delta < 8) Rank(index + delta)
    else None

  @inline def char: Char = (49 + index).toChar
  override def toString = char.toString
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < 8) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.y - 1)

  def fromChar(ch: Char): Option[Rank] = apply(ch.toInt - 49)

  val First   = new Rank(0)
  val Second  = new Rank(1)
  val Third   = new Rank(2)
  val Fourth  = new Rank(3)
  val Fifth   = new Rank(4)
  val Sixth   = new Rank(5)
  val Seventh = new Rank(6)
  val Eighth  = new Rank(7)

  val all = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth)
  val allReversed: List[Rank] = all.reverse
}
