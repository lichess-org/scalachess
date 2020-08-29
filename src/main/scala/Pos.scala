package chess

import scala.math.{ abs, max, min }

sealed case class Pos private (x0: Int, y0: Int, piotr: Char) {

  import Pos.posAt0

  val down: Option[Pos]         = posAt0(x0, y0 - 1)
  val left: Option[Pos]         = posAt0(x0 - 1, y0)
  val downLeft: Option[Pos]     = posAt0(x0 - 1, y0 - 1)
  val downRight: Option[Pos]    = posAt0(x0 + 1, y0 - 1)
  lazy val up: Option[Pos]      = posAt0(x0, y0 + 1)
  lazy val right: Option[Pos]   = posAt0(x0 + 1, y0)
  lazy val upLeft: Option[Pos]  = posAt0(x0 - 1, y0 + 1)
  lazy val upRight: Option[Pos] = posAt0(x0 + 1, y0 + 1)

  def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] =
    dir(this) map { p =>
      p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
    } getOrElse Nil

  def ?<(other: Pos): Boolean = x0 < other.x0
  def ?>(other: Pos): Boolean = x0 > other.x0
  def ?+(other: Pos): Boolean = y0 < other.y0
  def ?^(other: Pos): Boolean = y0 > other.y0
  def ?|(other: Pos): Boolean = x0 == other.x0
  def ?-(other: Pos): Boolean = y0 == other.y0

  def <->(other: Pos): Iterable[Pos] =
    min(x0, other.x0) to max(x0, other.x0) flatMap { posAt0(_, y0) }

  def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

  def onSameDiagonal(other: Pos): Boolean = color == other.color && xDist(other) == yDist(other)
  def onSameLine(other: Pos): Boolean     = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(x0 - other.x0)
  def yDist(other: Pos) = abs(y0 - other.y0)

  val file     = (97 + x0).toChar.toString
  val rank     = (y0 + 1).toString
  val key      = file + rank
  val color    = Color((x0 % 2 == 0) ^ (y0 % 2 == 0))
  val piotrStr = piotr.toString

  override val toString = key

  override val hashCode = x0 + 8 * y0
}

object Pos {
  val posCache = new Array[Pos](64)

  def posAt0(x: Int, y: Int): Option[Pos] =
    if (x < 0 || x > 7 || y < 0 || y > 7) None
    else posCache.lift(x + 8 * y)

  def posAt(key: String): Option[Pos] = allKeys get key

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String) = posAt(key) map (_.piotr)
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
    val pos = new Pos(x, y, piotr)
    posCache(x + 8 * y) = pos
    pos
  }

  val A1 = createPos(0, 0, 'a')
  val B1 = createPos(1, 0, 'b')
  val C1 = createPos(2, 0, 'c')
  val D1 = createPos(3, 0, 'd')
  val E1 = createPos(4, 0, 'e')
  val F1 = createPos(5, 0, 'f')
  val G1 = createPos(6, 0, 'g')
  val H1 = createPos(7, 0, 'h')
  val A2 = createPos(0, 1, 'i')
  val B2 = createPos(1, 1, 'j')
  val C2 = createPos(2, 1, 'k')
  val D2 = createPos(3, 1, 'l')
  val E2 = createPos(4, 1, 'm')
  val F2 = createPos(5, 1, 'n')
  val G2 = createPos(6, 1, 'o')
  val H2 = createPos(7, 1, 'p')
  val A3 = createPos(0, 2, 'q')
  val B3 = createPos(1, 2, 'r')
  val C3 = createPos(2, 2, 's')
  val D3 = createPos(3, 2, 't')
  val E3 = createPos(4, 2, 'u')
  val F3 = createPos(5, 2, 'v')
  val G3 = createPos(6, 2, 'w')
  val H3 = createPos(7, 2, 'x')
  val A4 = createPos(0, 3, 'y')
  val B4 = createPos(1, 3, 'z')
  val C4 = createPos(2, 3, 'A')
  val D4 = createPos(3, 3, 'B')
  val E4 = createPos(4, 3, 'C')
  val F4 = createPos(5, 3, 'D')
  val G4 = createPos(6, 3, 'E')
  val H4 = createPos(7, 3, 'F')
  val A5 = createPos(0, 4, 'G')
  val B5 = createPos(1, 4, 'H')
  val C5 = createPos(2, 4, 'I')
  val D5 = createPos(3, 4, 'J')
  val E5 = createPos(4, 4, 'K')
  val F5 = createPos(5, 4, 'L')
  val G5 = createPos(6, 4, 'M')
  val H5 = createPos(7, 4, 'N')
  val A6 = createPos(0, 5, 'O')
  val B6 = createPos(1, 5, 'P')
  val C6 = createPos(2, 5, 'Q')
  val D6 = createPos(3, 5, 'R')
  val E6 = createPos(4, 5, 'S')
  val F6 = createPos(5, 5, 'T')
  val G6 = createPos(6, 5, 'U')
  val H6 = createPos(7, 5, 'V')
  val A7 = createPos(0, 6, 'W')
  val B7 = createPos(1, 6, 'X')
  val C7 = createPos(2, 6, 'Y')
  val D7 = createPos(3, 6, 'Z')
  val E7 = createPos(4, 6, '0')
  val F7 = createPos(5, 6, '1')
  val G7 = createPos(6, 6, '2')
  val H7 = createPos(7, 6, '3')
  val A8 = createPos(0, 7, '4')
  val B8 = createPos(1, 7, '5')
  val C8 = createPos(2, 7, '6')
  val D8 = createPos(3, 7, '7')
  val E8 = createPos(4, 7, '8')
  val F8 = createPos(5, 7, '9')
  val G8 = createPos(6, 7, '!')
  val H8 = createPos(7, 7, '?')

  val all = posCache.toList

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
