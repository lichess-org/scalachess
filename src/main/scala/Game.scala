package chess

import format.pgn

case class Game(
    board: Board,
    player: Color = White,
    pgnMoves: String = "",
    clock: Option[Clock] = None,
    deads: List[(Pos, Piece)] = Nil,
    turns: Int = 0) {

  def apply(
    orig: Pos,
    dest: Pos,
    promotion: Option[PromotableRole] = None,
    lag: Int = 0): Valid[(Game, Move)] = for {
    move ← situation.move(orig, dest, promotion) map (_ withLag lag)
  } yield apply(move) -> move

  def apply(move: Move): Game = {
    val newGame = copy(
      board = move.finalizeAfter,
      player = !player,
      turns = turns + 1,
      clock = clock map (_ step move.lag),
      deads = (for {
        cpos ← move.capture
        cpiece ← board(cpos)
      } yield (cpos, cpiece) :: deads) | deads
    )
    val pgnMove = pgn.Dumper.move(situation, move, newGame.situation)
    newGame.copy(pgnMoves = pgnMoves.isEmpty.fold(
      pgnMove,
      pgnMoves + " " + pgnMove))
  }

  lazy val situation = Situation(board, player)

  lazy val allPieces: Iterable[(Pos, Piece, Boolean)] = (board.pieces map {
    case (pos, piece) ⇒ (pos, piece, false)
  }) ++ (deads map {
    case (pos, piece) ⇒ (pos, piece, true)
  })

  def pgnMovesList = pgnMoves.split(' ').toList

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
