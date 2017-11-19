package chess
package variant

import chess.Pos._

case object Horde extends Variant(
  id = 8,
  key = "horde",
  name = "Horde",
  shortName = "Horde",
  title = "Destroy the horde to win!",
  standardInitialPosition = false
) {

  /**
   * In Horde chess white advances against black with a horde of pawns.
   */
  lazy val pieces: Map[Pos, Piece] = {

    val frontPawns = List(Pos.B5, Pos.C5, Pos.F5, Pos.G5).map { _ -> White.pawn }

    val whitePawnsHoard = frontPawns ++ (for {
      x <- 1 to 8
      y <- 1 to 4
    } yield Pos.posAt(x, y) map (_ -> White.pawn)).flatten toMap

    val blackPieces = (for (y <- 7 to 8; x <- 1 to 8) yield {
      posAt(x, y) map { pos =>
        (pos, y match {
          case 8 => Black - backRank(x - 1)
          case 7 => Black.pawn
        })
      }
    }).flatten.toMap

    blackPieces ++ whitePawnsHoard
  }

  override val castles = Castles("kq")

  override val initialFen = "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"

  override def valid(board: Board, strict: Boolean) =
    board.kingPosOf(White).isEmpty && validSide(board, strict)(Black) && !pawnsOnPromotionRank(board, White)

  /** The game has a special end condition when white manages to capture all of black's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.piecesOf(White).isEmpty

  /**
   * P vs K + P where none of the pawns can move is a fortress draw
   */
  private def hordeClosedPosition(board: Board) = {
    board.actors.values.forall(actor => (actor.piece.is(Pawn) && actor.moves.isEmpty
      && InsufficientMatingMaterial.pawnBlockedByPawn(actor, board)) || actor.piece.is(King))
  }

  /**
   * In horde chess, black can win unless a fortress stalemate is unavoidable.
   *  Auto-drawing the game should almost never happen, but it did in https://lichess.org/xQ2RsU8N#121
   */
  override def insufficientWinningMaterial(board: Board) = hordeClosedPosition(board)

  /**
   * In horde chess, white cannot win on * V K or [BN]{2} v K or just one piece since they don't have a king
   * for support.
   */
  override def insufficientWinningMaterial(board: Board, color: Color) = {
    lazy val insufficientHordeMaterial = board.piecesOf(Color.white).size == 1 ||
      board.piecesOf(Color.white).size == 2 &&
      board.piecesOf(Color.white).forall(_._2.isMinor)
    color == Color.white && insufficientHordeMaterial || hordeClosedPosition(board)
  }

  override def isUnmovedPawn(color: Color, pos: Pos) =
    if (color.white) pos.y == 1 || pos.y == 2
    else pos.y == 7
}
