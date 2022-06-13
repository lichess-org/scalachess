package chess
package opening

final class FullOpening(
    val eco: String,
    val name: String,
    val fen: String
) {

  val (family: OpeningFamily, variation: Option[OpeningVariation]) = name.split(":", 2) match {
    case Array(f, v) => OpeningFamily(f.trim)    -> Some(OpeningVariation(v.takeWhile(',' !=).trim))
    case Array(f)    => OpeningFamily(f.trim)    -> None
    case _           => OpeningFamily(name.trim) -> None
  }

  def ecoName = s"$eco $name"

  override def toString = ecoName

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)
}

object FullOpening {

  case class AtPly(opening: FullOpening, ply: Int)
}

case class OpeningFamily(name: String)
case class OpeningVariation(name: String)
