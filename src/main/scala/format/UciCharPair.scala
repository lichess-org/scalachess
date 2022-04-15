package chess
package format

case class UciCharPair(a: Char, b: Char):

  override def toString = s"$a$b"

object UciCharPair:

  import implementation.*

  def apply(uci: Uci): UciCharPair =
    uci match
      case Uci.Move(orig, dest, None)       => UciCharPair(toChar(orig), toChar(dest))
      case Uci.Move(orig, dest, Some(role)) => UciCharPair(toChar(orig), toChar(dest.file, role))
      case Uci.Drop(role, pos) =>
        UciCharPair(
          toChar(pos),
          dropRole2charMap.getOrElse(role, voidChar)
        )

  private[format] object implementation:

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Pos.all
      .map { pos =>
        pos -> (pos.hashCode + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    val promotion2charMap: Map[(File, PromotableRole), Char] = for {
      (role, index) <- Role.allPromotable.zipWithIndex.to(Map)
      file          <- File.all
    } yield (file, role) -> (charShift + pos2charMap.size + index * 8 + file.index).toChar

    def toChar(file: File, prom: PromotableRole) =
      promotion2charMap.getOrElse(file -> prom, voidChar)

    val dropRole2charMap: Map[Role, Char] =
      Role.all
        .filterNot(King ==)
        .zipWithIndex
        .map { case (role, index) =>
          role -> (charShift + pos2charMap.size + promotion2charMap.size + index).toChar
        }
        .to(Map)
