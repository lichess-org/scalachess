package chess
package bitboard

import Bitboard.*

import scala.collection.mutable.ListBuffer

/** The idea is each variant can have its own movs generator.
  * Now I just want to finish standard variant first
  */
object StandardMovesGenerator:
  extension (f: Fen)
    // todo this function should return Either[InvalidPosition, List[Move]]
    def generate: List[Move] =
      // todo fix
      // if no king returns Left
      val king           = f.ourKing.get
      val enPassantMoves = f.state.epSquare.fold(List())(genEnPassant)
      val checkers       = f.checkers.get
      val moves = if checkers == Bitboard.empty then
        val targets = ~f.us
        genNonKing(targets) ++ genSafeKing(king, targets) ++ genCastling(king)
      else genEvasions(king, checkers)

      val blockers = f.sliderBlockers
      if blockers != Bitboard.empty || !f.state.epSquare.isDefined then
        moves.filter(m => f.isSafe(king, m, blockers))
      else moves

    def genEnPassant(ep: Pos): List[Move] =
      val pawns                                   = f.us & f.board.pawns & ep.pawnAttacks(!f.state.turn)
      val ff: Bitboard => Option[(Pos, Bitboard)] = bb => bb.lsb.map((_, bb & (bb - 1L)))
      List.unfold(pawns)(ff).map(Move.EnPassant(_, ep))

    def genNonKing(mask: Bitboard): List[Move] =
      genPawn(mask) ++ genKnight(mask) ++ genBishop(mask) ++ genRook(mask) ++ genQueen(mask)

    // this can still generate unsafe king moves
    def genSafeKing(king: Pos, mask: Bitboard): List[Move] =
      val targets = king.kingAttacks & mask
      for
        to <- targets.occupiedSquares
        if f.board.attacksTo(to, !f.state.turn).isEmpty
      yield Move.Normal(king, to, King, f.isOccupied(to))

    def genCastling(king: Pos): List[Move] =
      val firstRank = f.state.turn.backRank
      val rooks     = f.state.castlingRights & Bitboard.RANKS(firstRank.value)
      for
        rook <- rooks.occupiedSquares
        path = Bitboard.between(king, rook)
        if (path & f.occupied).isEmpty
        toRank   = if rook < king then Pos.C1 else Pos.G1
        kingTo   = toRank.combine(king)
        kingPath = Bitboard.between(king, kingTo) | (1L << kingTo.value) | (1L << king.value)
        safe = kingPath.occupiedSquares
          .map(f.board.attacksTo(_, !f.state.turn, f.occupied ^ (1L << king.value)).isEmpty)
          .forall(identity)
        if safe
      yield Move.Castle(king, rook)

    def genEvasions(king: Pos, checkers: Bitboard): List[Move] =
      // Checks by these sliding pieces can maybe be blocked.
      val sliders = checkers & (f.board.sliders)
      val attacked = sliders.occupiedSquares.foldRight(0L)((s, a) =>
        a | (Bitboard.RAYS(king.value)(s.value) ^ (1L << s.value))
      )
      val safeKings = genSafeKing(king, ~f.us & ~attacked)
      val blockers =
        if !checkers.moreThanOne then
          checkers.lsb.map(c => genNonKing(Bitboard.between(king, c) | checkers)).getOrElse(List())
        else List()
      safeKings ++ blockers

    def genKnight(mask: Bitboard): List[Move] =
      val knights = f.us & f.board.knights
      for
        from <- knights.occupiedSquares
        targets = Bitboard.knightAttacks(from) & mask
        to <- targets.occupiedSquares
      yield Move.Normal(from, to, Knight, f.isOccupied(to))

    def genBishop(mask: Bitboard): List[Move] =
      val bishops = f.us & f.board.bishops
      for
        from <- bishops.occupiedSquares
        targets = from.bishopAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield Move.Normal(from, to, Bishop, f.isOccupied(to))

    def genRook(mask: Bitboard): List[Move] =
      val rooks = f.us & f.board.rooks
      for
        from <- rooks.occupiedSquares
        targets = from.rookAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield Move.Normal(from, to, Rook, f.isOccupied(to))

    def genQueen(mask: Bitboard): List[Move] =
      val queens = f.us & f.board.queens
      for
        from <- queens.occupiedSquares
        targets = from.queenAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield Move.Normal(from, to, Queen, f.isOccupied(to))

    /** Generate all pawn moves except en passant This includes
      *   - captures
      *   - single square moves
      *   - double square moves
      * @mask:
      *   bitboard contains empty square or enemy pieces
      *
      *   TODO @mask includes enemy king now, which should not be because
      *   enemy cannot be captured by law
      */
    def genPawn(mask: Bitboard): List[Move] =
      val moves = ListBuffer[Move]()

      // pawn captures
      val capturers = f.us & f.board.pawns

      val s1: List[List[Move]] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(f.state.turn) & f.them & mask
        to <- targets.occupiedSquares
      yield genPawnMoves(from, to, true)

      // normal pawn moves
      val singleMoves = ~f.board.occupied & (if f.isWhiteTurn then ((f.board.white & f.board.pawns) << 8)
                                             else ((f.board.black & f.board.pawns) >>> 8)) & mask
      val doubleMoves =
        ~f.board.occupied & (if f.isWhiteTurn then (singleMoves << 8) else (singleMoves >>> 8))
          & Bitboard.RANKS(if f.isWhiteTurn then 3 else 4) & mask

      val s2: List[List[Move]] = for
        to <- singleMoves.occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -8 else 8)).get
      yield genPawnMoves(from, to, false)

      val s3: List[Move] = for
        to <- doubleMoves.occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -16 else 16)).get
      yield Move.Normal(from, to, Pawn, false)

      s1.flatten ++ s2.flatten ++ s3

    private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
      if from.rank == f.state.turn.seventhRank then
        List(Queen, Knight, Rook, Bishop).map(Move.Promotion(from, to, _, capture))
      else List(Move.Normal(from, to, Pawn, capture))
