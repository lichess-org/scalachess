package chess

import scala.math.{ min, max, abs }
import scala.collection.breakOut

sealed case class Pos private (x: Int, y: Int, piotr: Char) {

  import Pos.posAt

  val down: Option[Pos] = posAt(x, y - 1)
  val left: Option[Pos] = posAt(x - 1, y)
  val downLeft: Option[Pos] = posAt(x - 1, y - 1)
  val downRight: Option[Pos] = posAt(x + 1, y - 1)
  lazy val up: Option[Pos] = posAt(x, y + 1)
  lazy val right: Option[Pos] = posAt(x + 1, y)
  lazy val upLeft: Option[Pos] = posAt(x - 1, y + 1)
  lazy val upRight: Option[Pos] = posAt(x + 1, y + 1)

  def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] = dir(this) map { p =>
    p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
  } getOrElse Nil

  def ?<(other: Pos): Boolean = x < other.x
  def ?>(other: Pos): Boolean = x > other.x
  def ?+(other: Pos): Boolean = y < other.y
  def ?^(other: Pos): Boolean = y > other.y
  def ?|(other: Pos): Boolean = x == other.x
  def ?-(other: Pos): Boolean = y == other.y

  def <->(other: Pos): Iterable[Pos] =
    min(x, other.x) to max(x, other.x) flatMap { posAt(_, y) }

  def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

  def onSameDiagonal(other: Pos): Boolean = color == other.color && xDist(other) == yDist(other)
  def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(x - other.x)
  def yDist(other: Pos) = abs(y - other.y)

  val file = Pos xToString x
  val rank = y.toString
  val key = file + rank
  val color = Color((x % 2 == 0) ^ (y % 2 == 0))
  val piotrStr = piotr.toString

  override val toString = key

  override val hashCode = 8 * (y - 1) + (x - 1)
}

object Pos {
  val posCache = new Array[Some[Pos]](64)

  def posAt(x: Int, y: Int): Option[Pos] =
    if (x < 1 || x > 8 || y < 1 || y > 8) None
    else posCache(x + 8 * y - 9)

  def posAt(key: String): Option[Pos] = allKeys get key

  def xToString(x: Int) = (96 + x).toChar.toString

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String) = posAt(key) map (_.piotr)
  def doubleKeyToPiotr(key: String) = for {
    a <- keyToPiotr(key take 2)
    b <- keyToPiotr(key drop 2)
  } yield s"$a$b"
  def doublePiotrToKey(piotrs: String) = for {
    a <- piotr(piotrs.head)
    b <- piotr(piotrs(1))
  } yield s"${a.key}${b.key}"

  private[this] def createPos(x: Int, y: Int, piotr: Char): Pos = {
    val pos = new Pos(x, y, piotr)
    posCache(x + 8 * y - 9) = Some(pos)
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

  val all = posCache.toList.flatten

  val whiteBackrank = (A1 <-> H1).toList
  val blackBackrank = (A8 <-> H8).toList

  val allKeys: Map[String, Pos] = all.map { pos => pos.key -> pos }(breakOut)

  val allPiotrs: Map[Char, Pos] = all.map { pos => pos.piotr -> pos }(breakOut)
}
