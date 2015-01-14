package chess
package variant

case object ThreeCheck extends Variant(
  id = 5,
  key = "threeCheck",
  name = "Three-check",
  shortName = "3+",
  title = "Check your opponent 3 times to win the game") {

  override def hasMoveEffects = true

  override def addVariantEffect(move: Move) : Move = {
    val board = move.after updateHistory {
      _.withCheck(Color.White, move.after.checkWhite).withCheck(Color.Black, move.after.checkBlack)
    }

    move.copy(after = board)
  }

  override def specialEnd(situation: Situation) = situation.check && {
    val checks = situation.board.history.checkCount
    situation.color.fold(checks.white, checks.black) >= 3
  }
}
