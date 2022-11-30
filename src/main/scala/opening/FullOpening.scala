package chess
package opening

import chess.format.OpeningFen

final class FullOpening(
    val eco: Eco,
    val name: OpeningName,
    val fen: OpeningFen,
    val uci: UcisStr,
    val pgn: SansStr
):

  val (family: OpeningFamily, variation: Option[OpeningVariation]) = name.value.split(":", 2) match
    case Array(f, v) => OpeningFamily(OpeningName(f)) -> Some(OpeningVariation(v.takeWhile(',' !=).trim))
    case Array(f)    => OpeningFamily(OpeningName(f)) -> None
    case _           => OpeningFamily(name)           -> None

  lazy val nbMoves = uci.value.count(' ' ==) + 1
  lazy val lastUci = uci.value.split(' ').lastOption
  lazy val key     = FullOpening nameToKey name

  override def toString = name.value

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)

object FullOpening:

  case class AtPly(opening: FullOpening, ply: Int)

  object nameToKey:
    private val splitAccentRegex = "[\u0300-\u036f]".r
    private val multiSpaceRegex  = """\s+""".r
    private val badChars         = """[^\w\-]+""".r
    def apply(name: OpeningName) = OpeningKey {
      badChars.replaceAllIn(
        multiSpaceRegex.replaceAllIn(
          splitAccentRegex.replaceAllIn(
            // split an accented letter in the base letter and the accent
            java.text.Normalizer.normalize(name.value, java.text.Normalizer.Form.NFD),
            ""
          ),
          "_"
        ),
        ""
      )
    }
