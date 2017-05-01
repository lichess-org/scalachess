package chess
package variant

case object ThreeCheck extends Variant(
  id = 5,
  key = "threeCheck",
  name = "Three-check",
  shortName = "3check",
  title = "Check your opponent 3 times to win the game.",
  standardInitialPosition = true
) {

  def pieces = Standard.pieces

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 3+3 0 1"

  override def finalizeBoard(board: Board, uci: format.Uci, capture: Option[Piece]): Board =
    board updateHistory {
      _.withCheck(Color.White, board.checkWhite).withCheck(Color.Black, board.checkBlack)
    }

  override def specialEnd(situation: Situation) = situation.check && {
    val checks = situation.board.history.checkCount
    situation.color.fold(checks.white, checks.black) >= 3
  }

  /**
   * It's not possible to check or checkmate the opponent with only a king
   */
  override def insufficientWinningMaterial(board: Board, color: Color) =
    board.rolesOf(color) == List(King)

  // When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  // by the variant ending. However, no players can check if there are only kings remaining
  override def insufficientWinningMaterial(board: Board) = board.actors.forall(_._2.piece is King)
}
