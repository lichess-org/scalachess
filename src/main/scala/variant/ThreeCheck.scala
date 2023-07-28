package chess
package variant

import chess.format.EpdFen

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

  def pieces = Standard.pieces

  override val initialFen = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +0+0")

  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)

  override def finalizeBoard(board: Board, uci: format.Uci, capture: Option[Piece]): Board =
    board updateHistory:
      _.withCheck(Color.White, board.checkWhite).withCheck(Color.Black, board.checkBlack)

  override def specialEnd(situation: Situation) =
    situation.check.yes && {
      val checks = situation.board.history.checkCount
      situation.color.fold(checks.white, checks.black) >= 3
    }

  /** It's not possible to check or checkmate the opponent with only a king
    */
  override def opponentHasInsufficientMaterial(situation: Situation) =
    situation.board.kingsOnlyOf(!situation.color)

  // When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  // by the variant ending. However, no players can check if there are only kings remaining
  override def isInsufficientMaterial(board: Board) = board.kingsOnly
