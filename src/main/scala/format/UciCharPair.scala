package chess
package format

import cats.syntax.all.*

case class UciCharPair(a: Char, b: Char):

  override def toString = s"$a$b"

  def toUci: Uci =
    import UciCharPair.implementation.*
    val from: Square = unsafeCharToSquare(a) // :o

    char2squareMap.get(b) match
      case Some(sq) => Uci.Move(from, sq, None)
      case None =>
        char2promotionMap.get(b) match
          case Some((file, prom)) => Uci.Move(from, Square(file, lastRank(from)), Some(prom))
          case None               => Uci.Drop(unsafeCharToDropRole(b), from)

object UciCharPair:

  import implementation.*

  def apply(uci: Uci): UciCharPair =
    uci match
      case Uci.Move(orig, dest, None)       => UciCharPair(toChar(orig), toChar(dest))
      case Uci.Move(orig, dest, Some(role)) => UciCharPair(toChar(orig), toChar(dest.file, role))
      case Uci.Drop(role, square) =>
        UciCharPair(
          toChar(square),
          dropRole2charMap.getOrElse(role, voidChar)
        )

  object implementation:

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val square2charMap: Map[Square, Char] = Square.all.map { square =>
      square -> (square.hashCode + charShift).toChar
    }.toMap

    lazy val char2squareMap: Map[Char, Square] = square2charMap.map(_.swap)
    export char2squareMap.apply as unsafeCharToSquare

    inline def toChar(inline square: Square) = square2charMap.getOrElse(square, voidChar)

    val promotion2charMap: Map[(File, PromotableRole), Char] = for
      (role, index) <- Role.allPromotable.zipWithIndex.toMap
      file          <- File.all
    yield (file, role) -> (charShift + square2charMap.size + index * 8 + file.index).toChar

    lazy val char2promotionMap: Map[Char, (File, PromotableRole)] =
      promotion2charMap.map(_.swap)

    def toChar(file: File, prom: PromotableRole) =
      promotion2charMap.getOrElse(file -> prom, voidChar)

    val dropRole2charMap: Map[Role, Char] =
      Role.all
        .filterNot(King == _)
        .mapWithIndex: (role, index) =>
          role -> (charShift + square2charMap.size + promotion2charMap.size + index).toChar
        .toMap

    lazy val char2dropRoleMap: Map[Char, Role] = dropRole2charMap.map(_.swap)
    export char2dropRoleMap.apply as unsafeCharToDropRole

    private[format] def lastRank(from: Square): Rank =
      if from.rank == Rank.Second then Rank.First
      else Rank.Eighth
