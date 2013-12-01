package chess

import scala.concurrent.duration._

import format.pgn

case class Game(
    board: Board,
    player: Color = White,
    pgnMoves: List[String] = Nil,
    clock: Option[Clock] = None,
    deads: DeadPieces = Nil,
    turns: Int = 0) {

  def apply(
    orig: Pos,
    dest: Pos,
    promotion: Option[PromotableRole] = None,
    lag: FiniteDuration = 0.millis): Valid[(Game, Move)] = for {
    move ← situation.move(orig, dest, promotion) map (_ withLag lag)
  } yield apply(move) -> move

  def apply(move: Move): Game = {
    val newGame = copy(
      board = move.finalizeAfter,
      player = !player,
      turns = turns + 1,
      clock = clock map (_ step move.lag),
      deads = (move.capture flatMap board.apply).fold(deads)(_ :: deads)
    )
    val pgnMove = pgn.Dumper(situation, move, newGame.situation)
    newGame.copy(pgnMoves = pgnMoves.isEmpty.fold(
      List(pgnMove),
      pgnMoves :+ pgnMove))
  }

  lazy val situation = Situation(board, player)

  def allPieces: AllPieces = (board.pieces, deads)

  def withPgnMoves(x: List[String]) = copy(pgnMoves = x)

  /**
   * Halfmove clock: This is the number of halfmoves
   * since the last pawn advance or capture.
   * This is used to determine if a draw
   * can be claimed under the fifty-move rule.
   */
  def halfMoveClock: Int = board.history.positionHashes.size

  /**
   * Fullmove number: The number of the full move.
   * It starts at 1, and is incremented after Black's move.
   */
  def fullMoveNumber: Int = 1 + turns / 2

  def withBoard(b: Board) = copy(board = b)

  def updateBoard(f: Board ⇒ Board) = withBoard(f(board))

  def withPlayer(c: Color) = copy(player = c)

  def withTurns(t: Int) = copy(turns = t)
}

object Game {

  def apply(variant: Variant): Game = new Game(
    board = Board init variant
  )
}
