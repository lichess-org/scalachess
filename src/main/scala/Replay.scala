package chess

case class Replay(game: Game, moves: List[Move]) {

  def chronoMoves = moves.reverse
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil)
}
