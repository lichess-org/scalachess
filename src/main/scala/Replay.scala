package chess

import format.pgn.{ Reader, Tag }

case class Replay(game: Game, moves: List[Move]) {

  def chronoMoves = moves.reverse
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil)

  def apply(pgn: String, initialFen: Option[String], variant: Variant): Valid[Replay] =
    pgn.trim.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" flatMap { nonEmptyPgn ⇒
      Reader(
        nonEmptyPgn,
        List(
          initialFen map { fen ⇒ Tag(_.FEN, fen) },
          variant.some.filterNot(_.standard) map { v ⇒ Tag(_.Variant, v.name) }
        ).flatten
      )
    }
}
