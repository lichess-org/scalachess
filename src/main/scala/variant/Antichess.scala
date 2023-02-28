package chess
package variant

import chess.format.EpdFen
import bitboard.Bitboard.pawnAttacks

case object Antichess
    extends Variant(
      id = Variant.Id(6),
      key = Variant.LilaKey("antichess"),
      uciKey = Variant.UciKey("antichess"),
      name = "Antichess",
      shortName = "Anti",
      title = "Lose all your pieces (or get stalemated) to win the game.",
      standardInitialPosition = true
    ):

  def pieces = Standard.pieces

  // In antichess, it is not permitted to castle
  override val castles    = Castles.none
  override val initialFen = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")

  // In antichess, the king can't be put into check so we always return false
  override def kingSafety(m: Move): Boolean = true

  override def kingThreatened(board: Board, color: Color) = Check.No

  def validMoves(situation: Situation) =
    import situation.{ them, us, genNonKing, genEnPassant, board, genUnsafeKing, ourKings }
    val capturingMoves = captureMoves(situation)
    if capturingMoves.nonEmpty then capturingMoves
    else genNonKing(~board.occupied) ++ ourKings.flatMap(genUnsafeKing(_, ~board.occupied))

  def captureMoves(situation: Situation): List[Move] =
    import situation.{ them, us, genNonKing, genEnPassant, board, genUnsafeKing, ourKings }
    ourKings.flatMap(genUnsafeKing(_, them)) ++ genEnPassant(us & board.pawns) ++ genNonKing(them)

  override def valid(board: Board, strict: Boolean) =
    board.nbPieces >= 2 && board.nbPieces <= 32

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(situation: Situation): Option[Color] =
    specialEnd(situation) option situation.color

  override def specialEnd(situation: Situation) =
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    situation.board(situation.color).isEmpty || situation.legalMoves.isEmpty

  // In antichess, it is valuable for your opponent to have pieces.
  override def materialImbalance(board: Board): Int =
    board.allPieces.foldLeft(0) { case (acc, Piece(color, _)) =>
      acc + color.fold(-2, 2)
    }

  // In antichess, there is no checkmate condition therefore a player may only draw either by agreement,
  // blockade or stalemate. Only one player can win if the only remaining pieces are two knights
  override def opponentHasInsufficientMaterial(situation: Situation) =
    // Exit early if we are not in a situation with only knights
    situation.board.onlyKnights && {

      val whiteKnights = situation.board.white.occupiedSquares
      val blackKnights = situation.board.black.occupiedSquares

      // We consider the case where a player has two knights
      if (whiteKnights.size != 1 || blackKnights.size != 1) false
      else {
        for {
          whiteKnight <- whiteKnights.headOption
          blackKnight <- blackKnights.headOption
        } yield whiteKnight.isLight == blackKnight.isLight
      } getOrElse false
    }

  // No player can win if the only remaining pieces are opposing bishops on different coloured
  // diagonals. There may be pawns that are incapable of moving and do not attack the right color
  // of square to allow the player to force their opponent to capture their bishop, also resulting in a draw
  override def isInsufficientMaterial(board: Board) =
    // Exit early if we are not in a situation with only bishops and pawns
    if (board.bishops | board.pawns) != board.occupied then false
    else
      val whiteBishops = (board.white & board.bishops).occupiedSquares
      val blackBishops = (board.black & board.bishops).occupiedSquares
      if whiteBishops.map(_.isLight).to(Set).size != 1 ||
        blackBishops.map(_.isLight).to(Set).size != 1
      then false
      else
        val whitePawns = (board.white & board.pawns).occupiedSquares
        val blackPawns = (board.black & board.pawns).occupiedSquares
        (for {
          whiteBishopLight <- whiteBishops.headOption map (_.isLight)
          blackBishopLight <- blackBishops.headOption map (_.isLight)
        } yield whiteBishopLight != blackBishopLight && whitePawns.forall(
          pawnNotAttackable(_, blackBishopLight, board)
        ) && blackPawns.forall(pawnNotAttackable(_, whiteBishopLight, board)))
          .getOrElse(false)

  private def pawnNotAttackable(pawn: Pos, oppositeBishopLight: Boolean, board: Board): Boolean =
    // The pawn cannot attack a bishop or be attacked by a bishop
    val cannotAttackBishop = pawn.isLight != oppositeBishopLight
    InsufficientMatingMaterial.pawnBlockedByPawn(pawn, board) && cannotAttackBishop

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(_promotion: Option[PromotableRole]) = true

  override val roles = List(Rook, Knight, King, Bishop, Queen, Pawn)

  override val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)
