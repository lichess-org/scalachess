package chess

import format.pgn.{ Reader, Tag }

case class Replay(setup: Game, moves: List[Move], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(move: Move) = Replay(
    setup = setup,
    moves = move :: moves,
    state = state(move))

  def moveAtPly(ply: Int): Option[Move] = chronoMoves lift (ply - 1)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(moveStrs: List[String], initialFen: Option[String], variant: Variant): Valid[Replay] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" flatMap { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        List(
          initialFen map { fen => Tag(_.FEN, fen) },
          variant.some.filterNot(_.standard) map { v => Tag(_.Variant, v.name) }
        ).flatten
      )
    }
}
