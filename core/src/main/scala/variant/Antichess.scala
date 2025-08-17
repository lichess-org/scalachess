package chess
package variant

import chess.format.FullFen

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

  override val initialBoard: Board = Board.standard
  override def initialPieces: Map[Square, Piece] = initialBoard.pieceMap

  // In antichess, it is not permitted to castle
  override val castles: Castles = Castles.none
  override val initialFen: FullFen = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")

  // In antichess, the king can't be put into check so we always return false
  override def kingSafety(m: Move): Boolean = true

  override def kingThreatened(board: Board, color: Color): Check = Check.No

  override def validMoves(position: Position): List[Move] =
    import position.{ genNonKing, genUnsafeKing, ourKings }
    val capturingMoves = captureMoves(position)
    if capturingMoves.nonEmpty then capturingMoves
    else genNonKing(~position.occupied) ++ ourKings.flatMap(genUnsafeKing(_, ~position.occupied))

  override def validMovesAt(position: Position, square: Square): List[Move] =
    val captures = captureMoves(position)
    if captures.nonEmpty then captures.filter(_.orig == square)
    else super.validMovesAt(position, square)

  override def valid(position: Position, strict: Boolean): Boolean =
    position.nbPieces >= 2 && position.nbPieces <= 32

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(position: Position): Option[Color] =
    specialEnd(position).option(position.color)

  override def specialEnd(position: Position): Boolean =
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    position.us.isEmpty || position.legalMoves.isEmpty

  // In antichess, it is valuable for your opponent to have pieces.
  override def materialImbalance(board: Board): Int =
    board.fold(0): (acc, color, _) =>
      acc + color.fold(-2, 2)

  // In antichess, if the only remaining pieces are a knight each, then exactly one
  // player can win (depending on whose turn it is).

  override def hasInsufficientMaterial(position: Position, color: Color): Boolean =
    val isPlayer = color == position.color
    justOneKnightEach(position) && allOnSameColourSquares(position) != isPlayer

  // No player can win if the only remaining pieces are opposing bishops on different coloured
  // diagonals. There may be pawns that are incapable of moving and do not attack the right color
  // of square to allow the player to force their opponent to capture their bishop, also resulting in a draw
  override def isInsufficientMaterial(position: Position): Boolean =
    // Exit early if we are not in a board with only bishops and pawns
    if (position.bishops | position.pawns) != position.occupied then false
    else
      val whiteBishops = (position.white & position.bishops).squares
      val blackBishops = (position.black & position.bishops).squares
      if whiteBishops.map(_.isLight).to(Set).size != 1 ||
        blackBishops.map(_.isLight).to(Set).size != 1
      then false
      else
        val whitePawns = (position.white & position.pawns).squares
        val blackPawns = (position.black & position.pawns).squares
        (for
          whiteBishopLight <- whiteBishops.headOption.map(_.isLight)
          blackBishopLight <- blackBishops.headOption.map(_.isLight)
        yield whiteBishopLight != blackBishopLight && whitePawns.forall(
          pawnNotAttackable(_, blackBishopLight, position)
        ) && blackPawns.forall(pawnNotAttackable(_, whiteBishopLight, position)))
          .getOrElse(false)

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(_promotion: Option[PromotableRole]): Boolean = true

  override val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)

  private def captureMoves(position: Position): List[Move] =
    import position.{ them, us, genNonKing, genEnPassant, genUnsafeKing, ourKings }
    ourKings.flatMap(genUnsafeKing(_, them)) ++ genEnPassant(us & position.pawns) ++ genNonKing(them)

  private def pawnNotAttackable(pawn: Square, oppositeBishopLight: Boolean, position: Position): Boolean =
    // The pawn cannot attack a bishop or be attacked by a bishop
    val cannotAttackBishop = pawn.isLight != oppositeBishopLight
    InsufficientMatingMaterial.pawnBlockedByPawn(pawn, position) && cannotAttackBishop

  private def allOnSameColourSquares(position: Position): Boolean =
    Bitboard.lightSquares.isDisjoint(position.occupied) ||
      Bitboard.darkSquares.isDisjoint(position.occupied)

  private def justOneKnightEach(position: Position): Boolean =
    position.onlyKnights && position.white.count == 1 && position.black.count == 1
