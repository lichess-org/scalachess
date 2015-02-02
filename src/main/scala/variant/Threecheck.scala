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

  // When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  // by the variant ending
  override def drawsOnInsufficientMaterial = false

  // Although we do not draw on a traditional 'insufficient mating material', we do draw if only kings remain
  override def specialDraw(situation: Situation) = {
    situation.board.actors.forall(_._2.piece is King)
  }

}
