package chess
package variant

case object Antichess extends Variant(
  id = 6,
  key = "antichess",
  name = "Antichess",
  shortName = "Anti",
  title = "Lose all your pieces to win the game",
  standardInitialPosition = true) {

  override def pieces = {
    // In this chess variant, the king can ignore check and be captured, so we replace the normal king with the
    // antichess king
    convertPiecesFromStandard(super.pieces)
  }

  // In this variant, a player must capture if a capturing move is available
  override def validMoves(situation: Situation) = {
    val allMoves = super.validMoves(situation)
    val capturingMoves = allMoves mapValues (_.filter(_.captures)) filterNot (_._2.isEmpty)

    if (!capturingMoves.isEmpty) capturingMoves else allMoves
  }

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(situation: Situation): Option[Color] = if (specialEnd(situation)) Some(situation.color) else None

  override def specialEnd(situation: Situation) = {
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    situation.board.actorsOf(situation.color).isEmpty || situation.moves.isEmpty
  }

  // This mode has no checkmates
  override def drawsOnInsufficientMaterial = false

  override def specialDraw(situation: Situation) = {
    val actors = situation.board.actors
    if (actors.size != 2) false
    else actors.values.toList match {
      // No player can win if the only remaining pieces are two bishops of different colours on different coloured
      // diagonals
      case List(act1, act2) =>
        val bothPiecesAreBishops = act1.piece.is(Bishop) && act2.piece.is(Bishop)
        val notSamePlayerColour = (act1.color != act2.color)
        val notOnSameColouredDiagonals = act1.pos.color != act2.pos.color

        bothPiecesAreBishops && notOnSameColouredDiagonals && notSamePlayerColour
      case _ => false
    }
  }

  override def convertPiecesFromStandard(pieces: PieceMap): PieceMap = {
    pieces.mapValues {
      case Piece(color, King) => Piece(color, Antiking)
      case x                  => x
    }
  }

  override def valid(board: Board, strict: Boolean) = {
    // This variant cannot work with a 'normal' king as it assumes an AntiKing

    board.pieces.values.find(_.is(King)).isEmpty && {
      Color.all map board.rolesOf forall { roles =>
        (if (strict) List((roles count (_ == Pawn)) <= 8, roles.size <= 16) else Nil) forall identity
      }
    }
  }

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop | Antiking) => true
    case _ => false
  }

  override def roles = List(Rook, Knight, Antiking, Bishop, Queen, Pawn)

  override def promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, Antiking)
}
