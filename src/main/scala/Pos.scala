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

  def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

  def onSameDiagonal(other: Pos): Boolean =
    file.index - rank.index == other.file.index - other.rank.index || file.index + rank.index == other.file.index + other.rank.index
  def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)

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

  private[this] def createPos(x: Int, y: Int, piotr: Char): Pos = {
    val pos = new Pos((x - 1) + 8 * (y - 1))
    pos
  }

  val A1 = createPos(1, 1, 'a')
  val B1 = createPos(2, 1, 'b')
  val C1 = createPos(3, 1, 'c')
  val D1 = createPos(4, 1, 'd')
  val E1 = createPos(5, 1, 'e')
  val F1 = createPos(6, 1, 'f')
  val G1 = createPos(7, 1, 'g')
  val H1 = createPos(8, 1, 'h')
  val A2 = createPos(1, 2, 'i')
  val B2 = createPos(2, 2, 'j')
  val C2 = createPos(3, 2, 'k')
  val D2 = createPos(4, 2, 'l')
  val E2 = createPos(5, 2, 'm')
  val F2 = createPos(6, 2, 'n')
  val G2 = createPos(7, 2, 'o')
  val H2 = createPos(8, 2, 'p')
  val A3 = createPos(1, 3, 'q')
  val B3 = createPos(2, 3, 'r')
  val C3 = createPos(3, 3, 's')
  val D3 = createPos(4, 3, 't')
  val E3 = createPos(5, 3, 'u')
  val F3 = createPos(6, 3, 'v')
  val G3 = createPos(7, 3, 'w')
  val H3 = createPos(8, 3, 'x')
  val A4 = createPos(1, 4, 'y')
  val B4 = createPos(2, 4, 'z')
  val C4 = createPos(3, 4, 'A')
  val D4 = createPos(4, 4, 'B')
  val E4 = createPos(5, 4, 'C')
  val F4 = createPos(6, 4, 'D')
  val G4 = createPos(7, 4, 'E')
  val H4 = createPos(8, 4, 'F')
  val A5 = createPos(1, 5, 'G')
  val B5 = createPos(2, 5, 'H')
  val C5 = createPos(3, 5, 'I')
  val D5 = createPos(4, 5, 'J')
  val E5 = createPos(5, 5, 'K')
  val F5 = createPos(6, 5, 'L')
  val G5 = createPos(7, 5, 'M')
  val H5 = createPos(8, 5, 'N')
  val A6 = createPos(1, 6, 'O')
  val B6 = createPos(2, 6, 'P')
  val C6 = createPos(3, 6, 'Q')
  val D6 = createPos(4, 6, 'R')
  val E6 = createPos(5, 6, 'S')
  val F6 = createPos(6, 6, 'T')
  val G6 = createPos(7, 6, 'U')
  val H6 = createPos(8, 6, 'V')
  val A7 = createPos(1, 7, 'W')
  val B7 = createPos(2, 7, 'X')
  val C7 = createPos(3, 7, 'Y')
  val D7 = createPos(4, 7, 'Z')
  val E7 = createPos(5, 7, '0')
  val F7 = createPos(6, 7, '1')
  val G7 = createPos(7, 7, '2')
  val H7 = createPos(8, 7, '3')
  val A8 = createPos(1, 8, '4')
  val B8 = createPos(2, 8, '5')
  val C8 = createPos(3, 8, '6')
  val D8 = createPos(4, 8, '7')
  val E8 = createPos(5, 8, '8')
  val F8 = createPos(6, 8, '9')
  val G8 = createPos(7, 8, '!')
  val H8 = createPos(8, 8, '?')

  val all: List[Pos] = (0 to 63).map(new Pos(_)).toList

  val whiteBackrank = (A1 <-> H1).toList
  val blackBackrank = (A8 <-> H8).toList

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
