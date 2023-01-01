package chess

import cats.data.Validated
import cats.implicits.*
import bitboard.Bitboard
import bitboard.Bitboard.*

import chess.format.Uci
import chess.variant.Chess960

case class Situation(board: Board, color: Color):

  lazy val actors = board actorsOf color

  lazy val allTrustedMoves = board.variant.validMoves(this)

  lazy val moves: Map[Pos, List[Move]] =
    allTrustedMoves.groupBy(_.orig)

  lazy val allMoves: List[Move] = this.trustedMoves

  lazy val playerCanCapture: Boolean = allTrustedMoves.exists(_.captures)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None

  lazy val kingPos: Option[Pos] = board kingPosOf color

  lazy val check: Boolean = board checkOf color

  def checkSquare = if (check) kingPos else None

  inline def history = board.history

  inline def checkMate: Boolean = board.variant checkmate this

  inline def staleMate: Boolean = board.variant staleMate this

  inline def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  inline def variantEnd = board.variant specialEnd this

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  export board.history.canCastle

  def enPassantSquare: Option[Pos] =
    // Before potentially expensive move generation, first ensure some basic
    // conditions are met.
    // todo we can do better with bitboard
    history.lastMove match
      case Some(move: Uci.Move) =>
        if (
          move.dest.yDist(move.orig) == 2 &&
          board(move.dest).exists(_.is(Pawn)) &&
          List(
            move.dest.file.offset(-1),
            move.dest.file.offset(1)
          ).flatten.flatMap(board(_, color.passablePawnRank)).exists(_ == color.pawn)
        )
          allTrustedMoves.find(_.enpassant).map(_.dest)
        else None
      case _ => None

  def unary_! = copy(color = !color)

  // =======================================bitboard========================

  val ourKing                    = board.board.king(color)
  val theirKing                  = board.board.king(!color)
  val us: Bitboard               = board.board.byColor(color)
  def them: Bitboard             = board.board.byColor(!color)
  def checkers: Option[Bitboard] = ourKing.map(board.board.attacksTo(_, !color))
  def sliderBlockers: Bitboard   = board.board.sliderBlockers(color)
  def isWhiteTurn: Boolean       = color.white
  def isOccupied: Pos => Boolean = board.board.isOccupied

  def isSafe(king: Pos, move: Move, blockers: Bitboard): Boolean =
    if move.enpassant then
      val newOccupied = {
        board.occupied ^
          move.orig.bitboard ^ move.dest.withRankOf(move.orig).bitboard
      } | move.dest.bitboard
      (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)).isEmpty &&
      (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)).isEmpty
    else
      move.capture.isDefined || {
        !(us & blockers).contains(move.orig) || Bitboard.aligned(move.orig, move.dest, king)
      }

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board init variant, White)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)

  import scala.collection.mutable.ListBuffer

  extension (f: Situation)

    private def addCastlingMoves(prevMoves: List[Move], castlingMoves: List[Move]) =
      prevMoves ::: castlingMoves.filterNot { cm =>
        prevMoves.exists(m => m.orig == cm.orig && m.dest == cm.dest)
      }

    /** The moves without taking defending the king into account */
    def trustedMoves: List[Move] =
      val enPassantMoves = f.board.history.epSquare.fold(Nil)(genEnPassant)
      // println(s"passant $enPassantMoves")
      val targets        = ~f.us
      val withoutCastles = genNonKing(targets) ++ genUnsafeKing(targets)
      val movesWithoutEnPassant =
        if f.board.variant.allowsCastling then addCastlingMoves(withoutCastles, genCastling())
        else withoutCastles

      val moves = movesWithoutEnPassant ++ enPassantMoves

      // apply special effect
      if f.board.variant.hasMoveEffects then moves.map(f.board.variant.addVariantEffect(_))
      else moves

    def generateMoves: List[Move] =
      val enPassantMoves = f.board.history.epSquare.fold(Nil)(genEnPassant)
      // println(s"passant $enPassantMoves")
      val checkers = f.checkers.getOrElse(Bitboard.empty)
      // println(checkers)
      val movesWithoutEnpassant = if checkers == Bitboard.empty then
        val targets        = ~f.us
        val withoutCastles = genNonKing(targets) ++ genSafeKing(targets)
        if f.board.variant.allowsCastling then addCastlingMoves(withoutCastles, genCastling())
        else withoutCastles
      else genEvasions(checkers)
      val moves = movesWithoutEnpassant ++ enPassantMoves

      // king is no special in antichess so no need to verify it is in checked
      val safeMoves =
        if f.board.variant.antichess then moves
        else
          f.ourKing.fold(moves)(king =>
            val blockers = f.sliderBlockers
            if blockers != Bitboard.empty || !f.board.history.epSquare.isDefined then
              moves.filter(m => f.isSafe(king, m, blockers))
            else moves
          )

      // apply special effect
      if f.board.variant.hasMoveEffects then safeMoves.map(f.board.variant.addVariantEffect(_))
      else safeMoves

    // TODO we depend on the correctness of epSQuare here
    private def genEnPassant(ep: Pos): List[Move] =
      val pawns                                   = f.us & f.board.board.pawns & ep.pawnAttacks(!f.color)
      val ff: Bitboard => Option[(Pos, Bitboard)] = bb => bb.lsb.map((_, bb & (bb - 1L.bb)))
      List.unfold(pawns)(ff).mapFilter(enpassant(_, ep))

    private def genNonKing(mask: Bitboard): List[Move] =
      // println(s"mask $mask")
      genPawn(mask) ++ genKnight(mask) ++ genBishop(mask) ++ genRook(mask) ++ genQueen(mask)

    /** Generate all pawn moves except en passant
     *  This includes
      *   - captures
      *   - single square moves
      *   - double square moves
      * @mask:
      *   bitboard contains empty square or enemy pieces
      *
      *   TODO @mask includes enemy King now, which should not be because
      *   enemy King cannot be captured by law
      */
    private def genPawn(mask: Bitboard): List[Move] =
      val moves = ListBuffer[Move]()

      // println(s"turns ${f.color}")
      // our pawns or captures
      val capturers = f.us & f.board.board.pawns
      // println(s"capturers $capturers")

      val s1: List[Move] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(f.color) & f.them & mask
        to   <- targets.occupiedSquares
        move <- genPawnMoves(from, to, true)
      yield move
      // println(s"s1 $s1")

      // normal pawn moves
      val singleMoves = ~f.board.occupied & {
        if f.isWhiteTurn then (f.board.white & f.board.pawns) << 8
        else (f.board.black & f.board.pawns) >>> 8
      }

      // println(s"singleMoves $singleMoves")
      val doubleMoves =
        ~f.board.occupied &
          (if f.isWhiteTurn then singleMoves << 8 else singleMoves >>> 8) &
          (if f.board.variant.horde then Bitboard.rank(f.color.fourthRank) | Bitboard.rank(f.color.thirdRank)
           else Bitboard.rank(f.color.fourthRank))
      // println(s"doubleMoves $doubleMoves")

      val s2: List[Move] = for
        to   <- (singleMoves & mask).occupiedSquares
        from <- Pos.at(to.value + (if f.isWhiteTurn then -8 else 8)).toList
        move <- genPawnMoves(from, to, false)
      yield move
      // println(s"s2 $s2")

      val s3: List[Move] = for
        to   <- (doubleMoves & mask).occupiedSquares
        from <- Pos.at(to.value + (if f.isWhiteTurn then -16 else 16))
        move <- normalMove(from, to, Pawn, false)
      yield move

      s1 ++ s2 ++ s3

    private def genKnight(mask: Bitboard): List[Move] =
      val knights = f.us & f.board.knights
      for
        from <- knights.occupiedSquares
        targets = Bitboard.knightAttacks(from) & mask
        to   <- targets.occupiedSquares
        move <- normalMove(from, to, Knight, f.isOccupied(to))
      yield move

    private def genBishop(mask: Bitboard): List[Move] =
      val bishops = f.us & f.board.bishops
      for
        from <- bishops.occupiedSquares
        targets = from.bishopAttacks(f.board.occupied) & mask
        to   <- targets.occupiedSquares
        move <- normalMove(from, to, Bishop, f.isOccupied(to))
      yield move

    private def genRook(mask: Bitboard): List[Move] =
      val rooks = f.us & f.board.rooks
      for
        from <- rooks.occupiedSquares
        targets = from.rookAttacks(f.board.occupied) & mask
        to   <- targets.occupiedSquares
        move <- normalMove(from, to, Rook, f.isOccupied(to))
      yield move

    private def genQueen(mask: Bitboard): List[Move] =
      val queens = f.us & f.board.queens
      for
        from <- queens.occupiedSquares
        targets = from.queenAttacks(f.board.occupied) & mask
        to   <- targets.occupiedSquares
        move <- normalMove(from, to, Queen, f.isOccupied(to))
      yield move

    private def genEvasions(checkers: Bitboard): List[Move] =
      f.ourKing.fold(Nil)(king =>
        // Checks by these sliding pieces can maybe be blocked.
        val sliders = checkers & (f.board.sliders)
        // println(s"sliders: $sliders")
        val attacked = sliders.occupiedSquares.foldRight(Bitboard.empty)((s, a) =>
          a | (s.bitboard ^ Bitboard.ray(king, s))
        )
        val safeKings = genSafeKing(~f.us & ~attacked)
        // println(s"safeKings $safeKings")
        val blockers =
          if !checkers.moreThanOne then
            checkers.lsb.map(c => genNonKing(Bitboard.between(king, c) | checkers)).getOrElse(Nil)
          else Nil
        // println(s"blockers $blockers")
        safeKings ++ blockers
      )

    private def genUnsafeKing(mask: Bitboard): List[Move] =
      f.ourKing.fold(Nil)(king =>
        val targets = king.kingAttacks & mask
        targets.occupiedSquares.flatMap(to => normalMove(king, to, King, f.isOccupied(to)))
      )

    // this can still generate unsafe king moves
    private def genSafeKing(mask: Bitboard): List[Move] =
      f.ourKing.fold(Nil)(king =>
        val targets = king.kingAttacks & mask
        for
          to <- targets.occupiedSquares
          if f.board.board.attacksTo(to, !f.color).isEmpty
          move <- normalMove(king, to, King, f.isOccupied(to))
        yield move
      )

    // todo works with standard only
    // check king position
    private def genCastling(): List[Move] =
      import Castles.*
      f.ourKing.fold(Nil) { king =>
        def checkSafeSquare(pos: Pos, rookTo: Pos): Boolean =
          if f.board.variant.atomic then
            !f.board.board.atomicKingAttack(
              pos,
              f.color,
              (f.board.occupied ^ king.bitboard | rookTo.bitboard)
            )
          else f.board.board.attacksTo(pos, !f.color, f.board.occupied ^ king.bitboard).isEmpty

        // can castle but which side?
        if !f.board.history.castles.can(f.color).any || king.rank != f.color.backRank then Nil
        else
          val rooks = f.board.history.unmovedRooks & Bitboard.rank(f.color.backRank) & f.board.rooks
          for
            rook <- rooks.occupiedSquares
            toKingFile = if rook < king then File.C else File.G
            toRookFile = if rook < king then File.D else File.F
            kingTo     = Pos(toKingFile, king.rank)
            rookTo     = Pos(toRookFile, rook.rank)
            // calulate different path for standard vs chess960
            path =
              if f.board.variant.chess960 || f.board.variant.fromPosition
              then (Bitboard.between(king, rook) | Bitboard.between(king, kingTo))
              else Bitboard.between(king, rook)
            if (path & (f.board.occupied & ~rook.bitboard)).isEmpty
            kingPath = Bitboard.between(king, kingTo) | kingTo.bitboard | king.bitboard
            safe     = kingPath.occupiedSquares.forall(checkSafeSquare(_, rookTo))
            if safe
            moves <- castle(king, kingTo, rook, rookTo)
          yield moves
      }

    private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
      if from.rank == f.color.seventhRank then
        f.board.variant.promotableRoles.flatMap(promotion(from, to, _, capture))
      else normalMove(from, to, Pawn, capture).toList

    private def enpassant(orig: Pos, dest: Pos): Option[Move] =
      val capture = Pos(dest.file, orig.rank)
      f.board
        .taking(orig, dest, capture.some)
        .map(after =>
          Move(
            piece = f.color.pawn,
            orig = orig,
            dest = dest,
            situationBefore = f,
            after = after,
            capture = capture.some,
            castle = None,
            promotion = None,
            enpassant = true
          )
        )

    private def normalMove(orig: Pos, dest: Pos, role: Role, capture: Boolean): Option[Move] =
      val taken = if capture then Option(dest) else None
      val after =
        if (capture) then f.board.taking(orig, dest, taken)
        else f.board.move(orig, dest)
      after.map(board =>
        Move(
          piece = Piece(f.color, role),
          orig = orig,
          dest = dest,
          situationBefore = f,
          after = board,
          capture = taken,
          castle = None,
          promotion = None,
          enpassant = false
        )
      )

    private def promotion(orig: Pos, dest: Pos, promotion: PromotableRole, capture: Boolean): Option[Move] =
      val taken = if capture then Option(dest) else None
      val after: Option[Board] =
        if (capture) then f.board.taking(orig, dest, taken)
        else f.board.move(orig, dest)
      after
        .map(_.putOrReplace(Piece(f.color, promotion), dest))
        .map(board =>
          Move(
            piece = f.color.pawn,
            orig = orig,
            dest = dest,
            situationBefore = f,
            after = board,
            capture = taken,
            castle = None,
            promotion = Some(promotion),
            enpassant = false
          )
        )

    // for BC, we add a move where the king goes to the rook position
    // Here is the rules:
    // if the variant is Standard => 2 moves
    // if the variant is Chess960 => 1 move
    // if the variatn is not either of those two then
    //     if King and Rook are in standard position  => 2 moves
    //     else => 1 move
    // check logic in isChess960 function
    private def castle(king: Pos, kingTo: Pos, rook: Pos, rookTo: Pos): List[Move] =
      val after = for
        b1    <- f.board.take(king)
        b2    <- b1.take(rook)
        b3    <- b2.place(f.color.king, kingTo)
        after <- b3.place(f.color.rook, rookTo)
      yield after

      val isChess960 =
        if f.board.variant.standard then false
        else if f.board.variant.chess960 then true
        else king.file != File.E || !(rook.file == File.A || rook.file == File.H)

      val destInput = if (!isChess960) then List(kingTo, rook) else List(rook)

      for
        a            <- after.toList
        inputKingPos <- destInput // .filter(_ != king)
      yield Move(
        piece = f.color.king,
        orig = king,
        dest = inputKingPos,
        situationBefore = f,
        after = a,
        capture = None,
        castle = Move.Castle((king, kingTo), (rook, rookTo)).some,
        promotion = None,
        enpassant = false
      )
