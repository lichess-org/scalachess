package chess
package variant

import chess.format.FullFen

case object ThreeCheck
    extends Variant(
      id = Variant.Id(5),
      key = Variant.LilaKey("threeCheck"),
      uciKey = Variant.UciKey("3check"),
      name = "Three-check",
      shortName = "3check",
      title = "Check your opponent 3 times to win the game.",
      standardInitialPosition = true
    ):

  override val pieces: Map[Square, Piece] = Standard.pieces

  override val initialFen: FullFen = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +0+0")

  override def validMoves(position: Position): List[Move] =
    Standard.validMoves(position).map(updateCheckCount)

  override def valid(position: Position, strict: Boolean): Boolean = Standard.valid(position, strict)

  def updateCheckCount(move: Move): Move =
    move.copy(after = move.after.updateHistory:
      _.withCheck(Color.White, checkWhite(move.after.board))
        .withCheck(Color.Black, checkBlack(move.after.board)))

  override def specialEnd(position: Position): Boolean =
    position.check.yes && {
      val checks = position.history.checkCount
      position.color.fold(checks.white, checks.black) >= 3
    }

  /** It's not possible to check or checkmate the opponent with only a king
    */
  override def opponentHasInsufficientMaterial(position: Position): Boolean =
    position.kingsOnlyOf(!position.color)

  /**
  * When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  * by the variant ending. However, no players can check if there are only kings remaining
  */
  override def isInsufficientMaterial(position: Position): Boolean = position.kingsOnly
