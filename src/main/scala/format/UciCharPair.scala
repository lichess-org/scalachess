package chess
package format

case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  private type File = Int

  val pos2char: Map[Pos, Char] = Pos.all.map { pos =>
    pos -> pos.hashCode.toChar
  }

  val promotion2char: Map[(File, PromotableRole), Char] = (for {
    role <- Role.allPromotable
    file <- 1 to 8
  } yield (


  def apply(uci: Uci): UciCharPair = uci match {
    case Uci.Move(orig, dest, promotion) => ???
    case Uci.Drop(role, pos) => ???
  }
}
