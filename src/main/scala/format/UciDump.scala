package chess
package format

import pgn.{ Reader, Tag }

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String]): Valid[String] =
    pgn.trim.some.filter("" !=).fold[Valid[String]](success("")) { nonEmptyPgn ⇒
      Reader(
        nonEmptyPgn,
        initialFen.fold(List[Tag]()) { fen ⇒
          Tag(_.FEN, fen) :: Tag(_.Variant, chess.Variant.Chess960.name) :: Nil
        }
      ) map { _.chronoMoves map move mkString " " }
    }

  private def move(m: Move): String = m.castle.fold(
    m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
  ) {
      case ((kf, kt), (rf, rt)) if kf == kt ⇒ kf.key + rf.key
      case ((kf, kt), _) ⇒ kf.key + kt.key
    }
}
