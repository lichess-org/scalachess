package chess
package format

import pgn.{ Reader, Tag }

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String], variant: Variant): Valid[Seq[String]] =
    pgn.trim.some.filter(_.nonEmpty).fold[Valid[Seq[String]]](success(Nil)) { nonEmptyPgn ⇒
      Reader(
        nonEmptyPgn,
        List(
          initialFen map { fen ⇒ Tag(_.FEN, fen) },
          variant.some.filterNot(_.standard) map { v ⇒ Tag(_.Variant, v.name) }
        ).flatten
      ) map { _.chronoMoves map move(variant) }
    }

  def move(variant: Variant)(m: Move): String = m.castle.fold(
    m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
  ) {
      case ((kf, kt), (rf, rt)) if kf == kt || !variant.standard ⇒ kf.key + rf.key
      case ((kf, kt), _) ⇒ kf.key + kt.key
    }
}
