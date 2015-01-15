package chess
package variant

case object ThreeCheck extends Variant(
  id = 5,
  key = "threeCheck",
  name = "Three-check",
  shortName = "3+",
  title = "Check your opponent 3 times to win the game",
  standardInitialPosition = true) {

  override def finalizeBoard(board: Board): Board = board updateHistory {
      _.withCheck(Color.White, board.checkWhite).withCheck(Color.Black, board.checkBlack)
  }

  override def specialEnd(situation: Situation) = situation.check && {
    val checks = situation.board.history.checkCount
    situation.color.fold(checks.white, checks.black) >= 3
  }
}
