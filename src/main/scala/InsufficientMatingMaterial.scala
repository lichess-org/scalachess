package chess

/** Utility methods for helping to determine whether a situation is a draw or a draw
  * on a player flagging.
  *
  * See http://www.e4ec.org/immr.html
  */
object InsufficientMatingMaterial:

  def nonKingPieces(board: Board): PieceMap = board.pieces filter (_._2.role != King)

  def bishopsOnOppositeColors(board: Board) =
    (board.pieces collect { case (pos, Piece(_, Bishop)) => pos.isLight } toList).distinct
    .lengthCompare(2) == 0

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Pos, board: Board) =
    board(pawn).exists(p =>
      p.is(Pawn) &&
        Situation(board, p.color).generateMovesAt(pawn).isEmpty && {
          val blockingPosition = Actor.posAheadOfPawn(pawn, p.color)
          blockingPosition.flatMap(board(_)).exists(_.is(Pawn))
        }
    )

  /*
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(board: Board) =
    lazy val kingsAndBishopsOnly = board.allPieces forall { p =>
      (p is King) || (p is Bishop)
    }
    val kingsAndMinorsOnly = board.allPieces forall { p =>
      (p is King) || (p is Bishop) || (p is Knight)
    }

    kingsAndMinorsOnly && (board.allPieces.size <= 3 || (kingsAndBishopsOnly && !bishopsOnOppositeColors(
      board
    )))

  /*
   * Determines whether a color does not have mating material. In general:
   * King by itself is not mating material
   * King + knight mates against king + any(rook, bishop, knight, pawn)
   * King + bishop mates against king + any(bishop, knight, pawn)
   * King + bishop(s) versus king + bishop(s) depends upon bishop square colors
   */
  def apply(board: Board, color: Color) =

    val kingsAndMinorsOnlyOfColor = board.piecesOf(color) forall { p =>
      (p._2 is King) || (p._2 is Bishop) || (p._2 is Knight)
    }
    lazy val nonKingRolesOfColor  = board rolesOf color filter (King !=)
    lazy val rolesOfOpponentColor = board rolesOf !color

    kingsAndMinorsOnlyOfColor && (nonKingRolesOfColor.distinct match {
      case Nil => true
      case List(Knight) =>
        nonKingRolesOfColor.lengthCompare(
          1
        ) == 0 && !(rolesOfOpponentColor filter (King !=) exists (Queen !=))
      case List(Bishop) =>
        !(rolesOfOpponentColor.exists(r => r == Knight || r == Pawn) || bishopsOnOppositeColors(board))
      case _ => false
    })
