package chess

import scala.concurrent.duration._

import format.pgn

case class Game(
    board: Board,
    player: Color = White,
    pgnMoves: List[String] = Nil,
    clock: Option[Clock] = None,
    turns: Int = 0,
    startedAtTurn: Int = 0) {

  def apply(
    orig: Pos,
    dest: Pos,
    promotion: Option[PromotableRole] = None,
    lag: FiniteDuration = 0.millis): Valid[(Game, Move)] =
    situation.move(orig, dest, promotion) map (_ withLag lag) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newTurns = turns + 1
    val newGame = copy(
      board = move.finalizeAfter,
      player = !player,
      turns = newTurns,
      clock = clock map {
        case c: RunningClock => c step move.lag
        case c: PausedClock if (newTurns - startedAtTurn) == 2 => c.start.switch
        case c => c
      }
    )
    val pgnMove = pgn.Dumper(situation, move, newGame.situation)
    newGame.copy(pgnMoves = pgnMoves.isEmpty.fold(
      List(pgnMove),
      pgnMoves :+ pgnMove))
  }

  lazy val situation = Situation(board, player)

  def isStandardInit = board.pieces == chess.variant.Standard.pieces

  def withPgnMoves(x: List[String]) = copy(pgnMoves = x)

  def halfMoveClock: Int = board.history.halfMoveClock

  /**
   * Fullmove number: The number of the full move.
   * It starts at 1, and is incremented after Black's move.
   */
  def fullMoveNumber: Int = 1 + turns / 2

  def withBoard(b: Board) = copy(board = b)

  def updateBoard(f: Board => Board) = withBoard(f(board))

  def withPlayer(c: Color) = copy(player = c)

  def withTurns(t: Int) = copy(turns = t)
}

object Game {

  def apply(variant: chess.variant.Variant): Game = new Game(
    board = Board init variant
  )
}
