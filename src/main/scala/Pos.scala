package chess

import scala.math.{ abs, max, min }

case class Pos private (index: Int) extends AnyVal {

  def down: Option[Pos]      = Pos.at(file.index, rank.index - 1)
  def left: Option[Pos]      = Pos.at(file.index - 1, rank.index)
  def downLeft: Option[Pos]  = Pos.at(file.index - 1, rank.index - 1)
  def downRight: Option[Pos] = Pos.at(file.index + 1, rank.index - 1)
  def up: Option[Pos]        = Pos.at(file.index, rank.index + 1)
  def right: Option[Pos]     = Pos.at(file.index + 1, rank.index)
  def upLeft: Option[Pos]    = Pos.at(file.index - 1, rank.index + 1)
  def upRight: Option[Pos]   = Pos.at(file.index + 1, rank.index + 1)

  def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] =
    dir(this) map { p =>
      p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
    } getOrElse Nil

  def ?<(other: Pos): Boolean = file < other.file
  def ?>(other: Pos): Boolean = file > other.file
  def ?+(other: Pos): Boolean = rank < other.rank
  def ?^(other: Pos): Boolean = rank > other.rank
  def ?|(other: Pos): Boolean = file == other.file
  def ?-(other: Pos): Boolean = rank == other.rank

  def <->(other: Pos): Iterable[Pos] =
    min(file.index, other.file.index) to max(file.index, other.file.index) flatMap { Pos.at(_, rank.index) }

  def touches(other: Pos): Boolean = this == other || PosSet.kingAttack(this).has(other)

  def onSameDiagonal(other: Pos): Boolean =
    file.index - rank.index == other.file.index - other.rank.index || file.index + rank.index == other.file.index + other.rank.index
  def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)
  def dist(other: Pos) = max(xDist(other), yDist(other))

  def isLight: Boolean = (file.index + rank.index) % 2 == 1

  @inline def file = File of this
  @inline def rank = Rank of this

  def piotr: Char =
    if (index <= 25) (97 + index).toChar      // a ...
    else if (index <= 51) (39 + index).toChar // A ...
    else if (index <= 61) (index - 4).toChar  // 0 ...
    else if (index == 62) '!'
    else '?'
  def piotrStr = piotr.toString

  def key               = file.toString + rank.toString
  override def toString = key
}

object Pos {
  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < 64) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos = new Pos(file.index + 8 * rank.index)

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < 8 && 0 <= y && y < 8) Some(new Pos(x + 8 * y))
    else None

  def fromKey(key: String): Option[Pos] = allKeys get key

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String) = fromKey(key) map (_.piotr)
  def doubleKeyToPiotr(key: String) =
    for {
      a <- keyToPiotr(key take 2)
      b <- keyToPiotr(key drop 2)
    } yield s"$a$b"
  def doublePiotrToKey(piotrs: String) =
    for {
      a <- piotr(piotrs.head)
      b <- piotr(piotrs(1))
    } yield s"${a.key}${b.key}"

  @inline private[chess] def lsb(bitboard: Long): Option[Pos] =
    if (bitboard != 0) Some(new Pos(java.lang.Long.numberOfTrailingZeros(bitboard)))
    else None

  @inline private[chess] def msb(bitboard: Long): Option[Pos] =
    if (bitboard != 0) Some(new Pos(63 - java.lang.Long.numberOfLeadingZeros(bitboard)))
    else None

  val A1 = new Pos(0)
  val B1 = new Pos(1)
  val C1 = new Pos(2)
  val D1 = new Pos(3)
  val E1 = new Pos(4)
  val F1 = new Pos(5)
  val G1 = new Pos(6)
  val H1 = new Pos(7)
  val A2 = new Pos(8)
  val B2 = new Pos(9)
  val C2 = new Pos(10)
  val D2 = new Pos(11)
  val E2 = new Pos(12)
  val F2 = new Pos(13)
  val G2 = new Pos(14)
  val H2 = new Pos(15)
  val A3 = new Pos(16)
  val B3 = new Pos(17)
  val C3 = new Pos(18)
  val D3 = new Pos(19)
  val E3 = new Pos(20)
  val F3 = new Pos(21)
  val G3 = new Pos(22)
  val H3 = new Pos(23)
  val A4 = new Pos(24)
  val B4 = new Pos(25)
  val C4 = new Pos(26)
  val D4 = new Pos(27)
  val E4 = new Pos(28)
  val F4 = new Pos(29)
  val G4 = new Pos(30)
  val H4 = new Pos(31)
  val A5 = new Pos(32)
  val B5 = new Pos(33)
  val C5 = new Pos(34)
  val D5 = new Pos(35)
  val E5 = new Pos(36)
  val F5 = new Pos(37)
  val G5 = new Pos(38)
  val H5 = new Pos(39)
  val A6 = new Pos(40)
  val B6 = new Pos(41)
  val C6 = new Pos(42)
  val D6 = new Pos(43)
  val E6 = new Pos(44)
  val F6 = new Pos(45)
  val G6 = new Pos(46)
  val H6 = new Pos(47)
  val A7 = new Pos(48)
  val B7 = new Pos(49)
  val C7 = new Pos(50)
  val D7 = new Pos(51)
  val E7 = new Pos(52)
  val F7 = new Pos(53)
  val G7 = new Pos(54)
  val H7 = new Pos(55)
  val A8 = new Pos(56)
  val B8 = new Pos(57)
  val C8 = new Pos(58)
  val D8 = new Pos(59)
  val E8 = new Pos(60)
  val F8 = new Pos(61)
  val G8 = new Pos(62)
  val H8 = new Pos(63)

  val all: Array[Pos] = (0 to 63).map(new Pos(_)).toArray

  val allKeys: Map[String, Pos] = all
    .map { pos =>
      pos.key -> pos
    }
    .to(Map)

  val allPiotrs: Map[Char, Pos] = all
    .map { pos =>
      pos.piotr -> pos
    }
    .to(Map)
}
