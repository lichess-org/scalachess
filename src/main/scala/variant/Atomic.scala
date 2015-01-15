package chess
package variant

case object Atomic extends Variant(
  id = 7,
  key = "atomic",
  name = "Atomic",
  shortName = "Atom",
  title = "Nuke your opponent's king to win.",
  standardInitialPosition = true) {

  override def hasMoveEffects = true

  /** Moves which threaten to explode the opponent's king without exploding the player's own king */
  private def kingThreateningMoves(situation: Situation): Map[Pos, List[Move]] = {

    val moves = for {
      opponentKingPerimeter <- situation.board.kingPosOf(!situation.color) map (_.surroundingPositions)
      myKingPerimeter <- situation.kingPos map (_.surroundingPositions)

      kingAttackingMoves = situation.actors map {
        act =>
          val rawMoves = act.trustedMoves(true)

          act.pos -> rawMoves.filter(
            mv => opponentKingPerimeter.contains(mv.dest) &&
              mv.captures && (mv.piece isNot King) &&
              !myKingPerimeter.contains(mv.dest))
      } filter (!_._2.isEmpty)

    } yield kingAttackingMoves.toMap

    moves getOrElse Map.empty
  }

  /**
   * In atomic chess, a king cannot be threatened while it is in the perimeter of the other king as were the other player
   * to capture it, their own king would explode. This effectively makes a king invincible while connected with another
   * king.
   * */
  private def protectedByOtherKing(board: Board, to: Pos, color: Color) : Boolean =
    board.kingPosOf(color) map (_.surroundingPositions.contains(to)) getOrElse false

  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true): Boolean = {
    board.pieces exists {
      case (pos, piece) if piece.color == color && filter(piece) && piece.eyes(pos, to) && !protectedByOtherKing(board,to,color) =>
        (!piece.role.projection) || piece.role.dir(pos, to).exists {
          longRangeThreatens(board, pos, _, to)
        }
      case _ => false
    }
  }

  def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] = {
    (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }
  }

  override def validMoves(situation: Situation): Map[Pos, List[Move]] = {
    // In atomic chess, the pieces have the same roles as usual
    val usualMoves = super.validMoves(situation)

    /* However, it is illegal to make a capture that would result in your own king exploding. */
    val moves = for {
      surroundingKing <- situation.kingPos map (_.surroundingPositions)
      mvs1 = usualMoves.mapValues(_.filter(mv => !mv.captures || mv.captures && !surroundingKing.contains(mv.dest)))
      mvs2 = mvs1.filter(!_._2.isEmpty)
    } yield mvs2

    val kingSafeMoves = moves getOrElse usualMoves
    val kingExplodingMoves = kingThreateningMoves(situation)

    if (kingExplodingMoves.isEmpty)
      kingSafeMoves
    else {
      // A player may prioritise exploding the opponent's king over defending their own. This means they may ignore check
      // or move into a discovered check in order to explode their opponent's king if it doesn't explode their king in the
      // process.
      val maps = List(kingSafeMoves,kingExplodingMoves)
      mergeMap(maps){case (v1, v2) => v1 ++ v2}
    }
  }

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  private def explodeSurroundingPieces(move: Move): Move = {
    if (move.captures)
    {
      val surroundingPositions = move.dest.surroundingPositions
      val afterBoard = move.after
      val destination = move.dest

      val boardPieces = afterBoard.pieces

      // Pawns are immune (for some reason), but all pieces surrounding the captured piece and the capturing piece
      // itself explode
      val piecesToExplode = surroundingPositions.filter(boardPieces.get(_).fold(false)(_.isNot(Pawn))) + destination
      val afterExplosions = boardPieces -- piecesToExplode

      val newBoard = afterBoard withPieces afterExplosions
      move withAfter newBoard
    }
    else move
  }

  override def addVariantEffect(move: Move) : Move = explodeSurroundingPieces(move)

  /**
   * Since a king may walk into the path of another king, it is more difficult to win when your opponent only has a
   * king left.
   */
  private def insufficientAtomicWinningMaterial(board: Board) = {
    val whiteActors = board.actorsOf(White)
    val blackActors = board.actorsOf(Black)
    val allActors = board.actors
    lazy val allPieces = board.actors.values.map(_.piece).filter(_ isNot King)

    // One player must only have their king left
    if (whiteActors.size != 1 && blackActors.size != 1) false
    else {
      // You can mate with a queen, with just one of any other piece or with just a king
      allPieces.size == 1 && !allPieces.exists(_ is Queen) || allActors.size == 2
    }
  }

  override def specialDraw(situation: Situation) = {
    // Bishops on opposite coloured squares can never capture each other to cause a king to explode and a traditional
    // mate would be not be
    val board = situation.board
    InsufficientMatingMaterial.bishopsOnDifferentColor(board) || insufficientAtomicWinningMaterial(board)
  }

  // On insufficient mating material, a win may still be commonly achieved by exploding a piece next to a king
  override def drawsOnInsufficientMaterial = false

  /** Atomic chess has a special end where a king has been killed by exploding with an adjacent captured piece */
  override def specialEnd(situation: Situation) = situation.board.kingPos.size != 2
}

