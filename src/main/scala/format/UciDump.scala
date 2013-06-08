package chess
package format

import pgn.{ Reader, Tag }

object UciDump {

  // a2a4, b8c6
  def apply(pgn: String, initialFen: Option[String], variant: Option[Variant]): Valid[String] =
    pgn.trim.some.filter(_.nonEmpty).fold[Valid[String]](success("")) { nonEmptyPgn ⇒
      Reader(
        nonEmptyPgn,
        List(
          (initialFen map { fen ⇒ Tag(_.FEN, fen) }),
          (variant map { v ⇒ Tag(_.Variant, v.name) })
        ).flatten
      ) map {
          _.chronoMoves map { m ⇒
            m.castle.fold(
              m.orig.key + m.dest.key + m.promotion.fold("")(_.forsyth.toString)
            ) {
                case ((kf, kt), (rf, rt)) if kf == kt || variant == Some(Variant.Chess960) ⇒ kf.key + rf.key
                case ((kf, kt), _) ⇒ kf.key + kt.key
              }
          } mkString " "
        }
    }
}
