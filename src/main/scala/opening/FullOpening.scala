package chess
package opening

final class FullOpening(
    val eco: String,
    val name: String,
    val fen: String
) {

  val (family: OpeningFamily, variation: Option[OpeningVariation]) = name.split(":", 2) match {
    case Array(f, v) => OpeningFamily(f)    -> Some(OpeningVariation(v.takeWhile(',' !=)))
    case Array(f)    => OpeningFamily(f)    -> None
    case _           => OpeningFamily(name) -> None
  }

  def ecoName = s"$eco $name"

  override def toString = ecoName

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)
}

object FullOpening {

  case class AtPly(opening: FullOpening, ply: Int)
}

case class OpeningFamily(val name: String)
case class OpeningVariation(val name: String)
