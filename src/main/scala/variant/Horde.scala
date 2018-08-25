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

    val whitePawnsHorde = frontPawns ++ (for {
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

    blackPieces ++ whitePawnsHorde
  }

  override val castles = Castles("kq")

  override val initialFen = "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"

  override def valid(board: Board, strict: Boolean) =
    board.kingPosOf(White).isEmpty && validSide(board, strict)(Black) && !pawnsOnPromotionRank(board, White)

  /** The game has a special end condition when white manages to capture all of black's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.piecesOf(White).isEmpty

  /**
   * Any vs K + any where horde is stalemated and only king can move is a fortress draw
   * This does not consider imminent fortresses such as 8/p7/P7/8/8/P7/8/k7 b - -
   * nor does it consider contrived fortresses such as b7/pk6/P7/P7/8/8/8/8 b - -
   */
  private def hordeClosedPosition(board: Board) = {
    lazy val notKingBoard = board.kingPos.get(Color.black).flatMap(board.take).getOrElse(board)
    val hordePos = board.occupation(Color.white) // may include promoted pieces
    val mateInOne = hordePos.size == 1 && hordePos.forall(pos => pieceThreatened(board, Color.black, pos, (_ => true)))
    !mateInOne && notKingBoard.actors.values.forall(actor => actor.moves.isEmpty)
  }

  /**
   * In horde chess, black can win unless a fortress stalemate is unavoidable.
   *  Auto-drawing the game should almost never happen, but it did in https://lichess.org/xQ2RsU8N#121
   */
  override def insufficientWinningMaterial(board: Board) = hordeClosedPosition(board)

  /**
   * In horde chess, the horde cannot win on * V K or [BN]{2} v K or just one piece
   * since they lack a king for checkmate support.
   * Technically there are some positions where stalemate is unavoidable which
   * this method does not detect; however, such are trivial to premove.
   */
  override def insufficientWinningMaterial(board: Board, color: Color): Boolean = {
    lazy val fortress = hordeClosedPosition(board) // costly function call
    if (color == Color.white) {
      lazy val notKingPieces = InsufficientMatingMaterial.nonKingPieces(board)
      val horde = board.piecesOf(Color.white)
      lazy val hordeBishopSquareColors = horde.filter(_._2.is(Bishop)).toList.map(_._1.color).distinct
      lazy val hordeRoles = horde.map(_._2.role)
      lazy val army = board.piecesOf(Color.black)
      lazy val armyRooks = army.filter(p => p._2.is(Pawn) || p._2.is(Rook))
      lazy val armyBishops = army.filter(p => p._2.is(Pawn) || p._2.is(Bishop))
      lazy val armyKnights = army.filter(p => p._2.is(Pawn) || p._2.is(Knight))
      lazy val armyPawns = army.filter(_._2.is(Pawn))
      lazy val armyNonQueens = army.filter(_._2.isNot(Queen))
      lazy val armyNonRooks = army.filter(p => p._2.isNot(Queen) && p._2.isNot(Rook))
      lazy val armyNonBishops = army.filter(p => p._2.isNot(Queen) && p._2.isNot(Bishop))
      lazy val armyBishopSquareColors = armyBishops.toList.map(_._1.color).distinct
      if (horde.size == 1) {
        hordeRoles match {
          case List(Knight) => (army.size < 4 || armyNonRooks.isEmpty || armyNonBishops.isEmpty || armyPawns.isEmpty && (armyNonBishops.size + armyBishopSquareColors.size < 4))
          case List(Bishop) => (!notKingPieces.exists(_._2.role != Bishop) && notKingPieces.map(_._1.color).distinct.size == 1)
          case List(Rook) => (army.size < 3 || armyRooks.isEmpty || armyKnights.isEmpty)
          case _ => armyRooks.isEmpty
        }
      } else if ((hordeRoles.forall(_ == Bishop) && hordeBishopSquareColors.size == 1) && (armyBishops.size < 2 || (armyPawns.isEmpty && armyBishops.size == armyBishopSquareColors.size))) true
      else if ((horde.size == 2 && armyNonQueens.size <= 1) && (armyNonQueens.size == 0 || horde.forall(_._2.isMinor))) true
      else if (notKingPieces.map(_._2.role).distinct == List(Bishop) && !InsufficientMatingMaterial.bishopsOnDifferentColor(board)) true
      else fortress
    } else fortress
  }

  override def isUnmovedPawn(color: Color, pos: Pos) =
    if (color.white) pos.y == 1 || pos.y == 2
    else pos.y == 7
}
