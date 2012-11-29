package chess
package format

import pgn.{ Reader, Tag }

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String]): Valid[String] = for {
    nonEmptyPgn ← pgn.trim.some filter ("" !=) toValid "Empty pgn"
    moves ← Reader(
      nonEmptyPgn,
      initialFen.fold(List[Tag]()) { fen ⇒
        Tag(_.FEN, fen) :: Tag(_.Variant, chess.Variant.Chess960.name) :: Nil
      }
    ) map (_.chronoMoves map move mkString " ")
  } yield moves

  private def move(m: Move): String =
    m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
}
