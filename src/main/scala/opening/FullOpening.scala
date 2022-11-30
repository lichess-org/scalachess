package chess
package opening

import chess.format.Fen

final class FullOpening(
    val eco: String,
    val name: String,
    val fen: Fen,
    val uci: String,
    val pgn: String
):

  val (family: OpeningFamily, variation: Option[OpeningVariation]) = name.split(":", 2) match
    case Array(f, v) => OpeningFamily(f.trim)    -> Some(OpeningVariation(v.takeWhile(',' !=).trim))
    case Array(f)    => OpeningFamily(f.trim)    -> None
    case _           => OpeningFamily(name.trim) -> None

  lazy val nbMoves = uci.count(' ' ==) + 1
  lazy val lastUci = uci.split(' ').lastOption
  lazy val key     = FullOpening nameToKey name

  override def toString = name

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)

object FullOpening:

  type Key = String

  case class AtPly(opening: FullOpening, ply: Int)

  object nameToKey:
    private val splitAccentRegex = "[\u0300-\u036f]".r
    private val multiSpaceRegex  = """\s+""".r
    private val badChars         = """[^\w\-]+""".r
    def apply(name: String): Key =
      badChars.replaceAllIn(
        multiSpaceRegex.replaceAllIn(
          splitAccentRegex.replaceAllIn(
            // split an accented letter in the base letter and the accent
            java.text.Normalizer.normalize(name, java.text.Normalizer.Form.NFD),
            ""
          ),
          "_"
        ),
        ""
      )

case class OpeningFamily(name: String):
  lazy val key = FullOpening nameToKey name
case class OpeningVariation(name: String)
