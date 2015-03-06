package chess
package variant

import chess.Pos._

case object Horde extends Variant(
  id = 8,
  key = "horde",
  name = "Horde",
  shortName = "horde",
  title = "Destroy the horde to win!",
  standardInitialPosition = false) {

  /**
   * In Horde chess black advances against white with a horde of pawns.
   */
  override def pieces: Map[Pos, Piece] = {

    // In horde chess, black has a block of pawns for their first 4 rows but swaps the pawns on d8 and e8 to d4 and e4
    val frontTwoPawns = List(Pos.D4 -> Black.pawn, Pos.E4 -> Black.pawn)

    val blackPawnsHoard = frontTwoPawns ++ (for {
      x <- 1 to 8; y <- 5 to 8
      if (x,y) != (4,8) && (x,y) != (5,8)
    } yield Pos.posAt(x, y) map (_ -> Black.pawn)).flatten toMap

    val whitePieces = (for (y <- 1 to 2; x <- 1 to 8) yield {
      posAt(x, y) map { pos =>
        (pos, y match {
          case 1 => White - backRank(x - 1)
          case 2 => White.pawn
        })
      }
    }).flatten.toMap

    whitePieces ++ blackPawnsHoard
  }

  /** The game has a special end condition when white manages to capture all of black's pawns */
  override def specialEnd(situation: Situation) = situation.board.piecesOf(Black).isEmpty

}