package chess

import scala.math.{ abs, max, min }

opaque type Pos = Int
object Pos extends OpaqueInt[Pos]:
  extension (p: Pos)
    inline def down: Option[Pos]      = Pos.at(file.value, rank.value - 1)
    inline def left: Option[Pos]      = Pos.at(file.value - 1, rank.value)
    inline def downLeft: Option[Pos]  = Pos.at(file.value - 1, rank.value - 1)
    inline def downRight: Option[Pos] = Pos.at(file.value + 1, rank.value - 1)
    inline def up: Option[Pos]        = Pos.at(file.value, rank.value + 1)
    inline def right: Option[Pos]     = Pos.at(file.value + 1, rank.value)
    inline def upLeft: Option[Pos]    = Pos.at(file.value - 1, rank.value + 1)
    inline def upRight: Option[Pos]   = Pos.at(file.value + 1, rank.value + 1)

    def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
    def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)
    def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] =
      dir(p) map { p =>
        p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
      } getOrElse Nil

    def ?<(other: Pos): Boolean = file < other.file
    def ?>(other: Pos): Boolean = file > other.file
    def ?+(other: Pos): Boolean = rank < other.rank
    def ?^(other: Pos): Boolean = rank > other.rank
    def ?|(other: Pos): Boolean = file == other.file
    def ?-(other: Pos): Boolean = rank == other.rank

    def <->(other: Pos): Iterable[Pos] =
      min(file.value, other.file.value) to max(file.value, other.file.value) flatMap { Pos.at(_, rank.value) }

    def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

    def onSameDiagonal(other: Pos): Boolean =
      file.value - rank.value == other.file.value - other.rank.value || file.value + rank.value == other.file.value + other.rank.value
    def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

    def xDist(other: Pos) = abs(p.file.value - other.file.value)
    def yDist(other: Pos) = abs(p.rank.value - other.rank.value)

    def isLight: Boolean = (file.value + rank.value) % 2 == 1

    inline def file: File = File of p
    inline def rank: Rank = Rank of p

    def toChar: Char =
      if (p <= 25) (97 + p).toChar      // a ...
      else if (p <= 51) (39 + p).toChar // A ...
      else if (p <= 61) (p - 4).toChar  // 0 ...
      else if (p == 62) '!'
      else '?'

    def key = s"${p.file.char}${p.rank.char}"
  end extension

  def apply(file: File, rank: Rank): Pos = file.value + 8 * rank.value

  inline def at(inline x: Int, inline y: Int): Option[Pos] =
    if (0 <= x && x < 8 && 0 <= y && y < 8) Some(x + 8 * y)
    else None

  def at(index: Int): Option[Pos] =
    if (0 <= index && index < 64) Some(index)
    else None

  def fromKey(key: String): Option[Pos] = allKeys get key

  def fromChar(c: Char): Option[Pos] = charMap get c

  def keyToChar(key: String) = fromKey(key).map(_.toChar)
  def doubleKeyToChars(key: String) = for
    a <- keyToChar(key take 2)
    b <- keyToChar(key drop 2)
  yield s"$a$b"
  def doubleCharToKey(chars: String) = for
    a <- fromChar(chars.head)
    b <- fromChar(chars(1))
  yield s"${a.key}${b.key}"

  val A1: Pos = 0
  val B1: Pos = 1
  val C1: Pos = 2
  val D1: Pos = 3
  val E1: Pos = 4
  val F1: Pos = 5
  val G1: Pos = 6
  val H1: Pos = 7
  val A2: Pos = 8
  val B2: Pos = 9
  val C2: Pos = 10
  val D2: Pos = 11
  val E2: Pos = 12
  val F2: Pos = 13
  val G2: Pos = 14
  val H2: Pos = 15
  val A3: Pos = 16
  val B3: Pos = 17
  val C3: Pos = 18
  val D3: Pos = 19
  val E3: Pos = 20
  val F3: Pos = 21
  val G3: Pos = 22
  val H3: Pos = 23
  val A4: Pos = 24
  val B4: Pos = 25
  val C4: Pos = 26
  val D4: Pos = 27
  val E4: Pos = 28
  val F4: Pos = 29
  val G4: Pos = 30
  val H4: Pos = 31
  val A5: Pos = 32
  val B5: Pos = 33
  val C5: Pos = 34
  val D5: Pos = 35
  val E5: Pos = 36
  val F5: Pos = 37
  val G5: Pos = 38
  val H5: Pos = 39
  val A6: Pos = 40
  val B6: Pos = 41
  val C6: Pos = 42
  val D6: Pos = 43
  val E6: Pos = 44
  val F6: Pos = 45
  val G6: Pos = 46
  val H6: Pos = 47
  val A7: Pos = 48
  val B7: Pos = 49
  val C7: Pos = 50
  val D7: Pos = 51
  val E7: Pos = 52
  val F7: Pos = 53
  val G7: Pos = 54
  val H7: Pos = 55
  val A8: Pos = 56
  val B8: Pos = 57
  val C8: Pos = 58
  val D8: Pos = 59
  val E8: Pos = 60
  val F8: Pos = 61
  val G8: Pos = 62
  val H8: Pos = 63

  val all: List[Pos] = Pos from (0 to 63).toList

  val whiteBackrank = (A1 <-> H1).toList
  val blackBackrank = (A8 <-> H8).toList

  val allKeys: Map[String, Pos] = all
    .map { pos =>
      pos.key -> pos
    }
    .to(Map)

  val charMap: Map[Char, Pos] = all
    .map { pos =>
      pos.toChar -> pos
    }
    .to(Map)
