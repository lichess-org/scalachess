package chess

class Replay private[chess] (val game: Game, val moves: List[Move]) {

  def chronoMoves = moves.reverse
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil)
}
