package chess

import scala.collection.mutable.ListBuffer
import cats.data.Validated
import cats.implicits.*

import bitboard.Bitboard
import bitboard.Bitboard.*

import chess.format.Uci
import chess.variant.Chess960

case class Situation(board: Board, color: Color):

  lazy val actors = board actorsOf color

  lazy val legalMoves = board.variant.validMoves(this)

  lazy val moves: Map[Pos, List[Move]] =
    legalMoves.groupBy(_.orig)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None

  lazy val check: Boolean = board checkOf color

  def checkSquare = if (check) ourKing else None

  def piecesOf(c: Color): Map[Pos, Piece] = board.piecesOf(c)

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
          legalMoves.find(_.enpassant).map(_.dest)
        else None
      case _ => None

  def unary_! = copy(color = !color)

  // ========================bitboard===========================

  lazy val ourKing: Option[Pos]       = board.board.king(color)
  lazy val theirKing: Option[Pos]     = board.board.king(!color)
  lazy val us: Bitboard               = board.board.byColor(color)
  lazy val them: Bitboard             = board.board.byColor(!color)
  lazy val checkers: Option[Bitboard] = ourKing.map(board.board.attacksTo(_, !color))
  lazy val sliderBlockers: Bitboard   = board.board.sliderBlockers(color)
  lazy val isWhiteTurn: Boolean       = color.white
  lazy val isOccupied: Pos => Boolean = board.board.isOccupied

  /** The moves without taking defending the king into account */
  lazy val generateMoves: List[Move] =
    val targets = ~us
    val moves   = genNonKing(targets) ++ genKings(targets) ++ genEnPassant(us & board.pawns)

    // apply special effect
    if board.variant.hasMoveEffects then moves.map(board.variant.addVariantEffect(_))
    else moves

  // Test generateMovesAt(pos) = generateMoves.filter(_.orig == pos)
  def generateMovesAt(pos: Pos): List[Move] =
    board.apply(pos).fold(Nil) { piece =>
      if piece.color != color then Nil
      else
        val targets = ~us
        val bb      = pos.bitboard
        piece.role match
          case Pawn   => genEnPassant(us & bb) ++ genPawn(bb, targets)
          case Knight => genKnight(us & bb, targets)
          case Bishop => genBishop(us & bb, targets)
          case Rook   => genRook(us & bb, targets)
          case Queen  => genQueen(us & bb, targets)
          case King   => genKings(targets)
    }

  private def genKings(mask: Bitboard) =
    val withoutCastles = ourKing.fold(Nil)(genUnsafeKing(_, mask))
    if board.variant.allowsCastling then withoutCastles ::: genCastling()
    else withoutCastles

  // TODO we depend on the correctness of epSQuare here
  private def genEnPassant(pawns: Bitboard): List[Move] =
    board.history.epSquare.fold(Nil)(ep =>
      val pawnsCanEnPassant                       = pawns & ep.pawnAttacks(!color)
      val ff: Bitboard => Option[(Pos, Bitboard)] = bb => bb.lsb.map((_, bb & (bb - 1L.bb)))
      List.unfold(pawnsCanEnPassant)(ff).mapFilter(enpassant(_, ep))
    )

  private def genNonKing(mask: Bitboard): List[Move] =
    genPawn(us & board.pawns, mask) ++ genKnight(us & board.knights, mask) ++
      genBishop(us & board.bishops, mask) ++ genRook(us & board.rooks, mask) ++
      genQueen(us & board.queens, mask)

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
  private def genPawn(pawns: Bitboard, mask: Bitboard): List[Move] =
    // our pawns which are called captures
    val capturers = pawns

    val s1: List[Move] = for
      from <- capturers.occupiedSquares
      targets = from.pawnAttacks(color) & them & mask
      to   <- targets.occupiedSquares
      move <- genPawnMoves(from, to, true)
    yield move

    // normal pawn moves
    val singleMoves = ~board.occupied & {
      // if isWhiteTurn then pawns << 8
      // else pawns >>> 8
      if isWhiteTurn then (board.white & board.pawns) << 8
      else (board.black & board.pawns) >>> 8
    }

    val doubleMoves =
      ~board.occupied &
        (if isWhiteTurn then singleMoves << 8 else singleMoves >>> 8) &
        (if board.variant.horde then Bitboard.rank(color.fourthRank) | Bitboard.rank(color.thirdRank)
         else Bitboard.rank(color.fourthRank))

    val s2: List[Move] = for
      to   <- (singleMoves & mask).occupiedSquares
      from <- Pos.at(to.value + (if isWhiteTurn then -8 else 8)).toList
      move <- genPawnMoves(from, to, false)
    yield move

    val s3: List[Move] = for
      to   <- (doubleMoves & mask).occupiedSquares
      from <- Pos.at(to.value + (if isWhiteTurn then -16 else 16))
      move <- normalMove(from, to, Pawn, false)
    yield move

    s1 ++ s2 ++ s3

  private def genKnight(knights: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- knights.occupiedSquares
      targets = Bitboard.knightAttacks(from) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Knight, isOccupied(to))
    yield move

  private def genBishop(bishops: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- bishops.occupiedSquares
      targets = from.bishopAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Bishop, isOccupied(to))
    yield move

  private def genRook(rooks: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- rooks.occupiedSquares
      targets = from.rookAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Rook, isOccupied(to))
    yield move

  private def genQueen(queens: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- queens.occupiedSquares
      targets = from.queenAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Queen, isOccupied(to))
    yield move

  private def genEvasions(checkers: Bitboard): List[Move] =
    ourKing.fold(Nil)(king =>
      // Checks by these sliding pieces can maybe be blocked.
      val sliders = checkers & (board.sliders)
      val attacked =
        sliders.occupiedSquares.foldRight(Bitboard.empty)((s, a) => a | (s.bitboard ^ Bitboard.ray(king, s)))
      val safeKings = genSafeKing(~us & ~attacked)
      val blockers =
        if !checkers.moreThanOne then
          checkers.lsb.map(c => genNonKing(Bitboard.between(king, c) | checkers)).getOrElse(Nil)
        else Nil
      safeKings ++ blockers
    )

  private def genUnsafeKing(king: Pos, mask: Bitboard): List[Move] =
    val targets = king.kingAttacks & mask
    targets.occupiedSquares.flatMap(to => normalMove(king, to, King, isOccupied(to)))

  // this can still generate unsafe king moves
  private def genSafeKing(mask: Bitboard): List[Move] =
    ourKing.fold(Nil)(king =>
      val targets = king.kingAttacks & mask
      for
        to <- targets.occupiedSquares
        if board.board.attacksTo(to, !color).isEmpty
        move <- normalMove(king, to, King, isOccupied(to))
      yield move
    )

  // todo works with standard only
  // check king position
  private def genCastling(): List[Move] =
    import Castles.*
    ourKing.fold(Nil) { king =>
      def checkSafeSquare(pos: Pos, rookTo: Pos): Boolean =
        if board.variant.atomic then
          !board.board.atomicKingAttack(
            pos,
            color,
            (board.occupied ^ king.bitboard | rookTo.bitboard)
          )
        else board.board.attacksTo(pos, !color, board.occupied ^ king.bitboard).isEmpty

      // can castle but which side?
      if !board.history.castles.can(color).any || king.rank != color.backRank then Nil
      else
        val rooks = board.history.unmovedRooks & Bitboard.rank(color.backRank) & board.rooks
        for
          rook <- rooks.occupiedSquares
          toKingFile = if rook < king then File.C else File.G
          toRookFile = if rook < king then File.D else File.F
          kingTo     = Pos(toKingFile, king.rank)
          rookTo     = Pos(toRookFile, rook.rank)
          // calulate different path for standard vs chess960
          path =
            if board.variant.chess960 || board.variant.fromPosition
            then (Bitboard.between(king, rook) | Bitboard.between(king, kingTo))
            else Bitboard.between(king, rook)
          if (path & (board.occupied & ~rook.bitboard)).isEmpty
          kingPath = Bitboard.between(king, kingTo) | kingTo.bitboard | king.bitboard
          safe     = kingPath.occupiedSquares.forall(checkSafeSquare(_, rookTo))
          if safe
          moves <- castle(king, kingTo, rook, rookTo)
        yield moves
    }

  private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
    if from.rank == color.seventhRank then
      board.variant.promotableRoles.flatMap(promotion(from, to, _, capture))
    else normalMove(from, to, Pawn, capture).toList

  private def enpassant(orig: Pos, dest: Pos): Option[Move] =
    val capture = Pos(dest.file, orig.rank)
    board
      .taking(orig, dest, capture.some)
      .map(after =>
        Move(
          piece = color.pawn,
          orig = orig,
          dest = dest,
          situationBefore = this,
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
      if (capture) then board.taking(orig, dest, taken)
      else board.move(orig, dest)
    after.map(board =>
      Move(
        piece = Piece(color, role),
        orig = orig,
        dest = dest,
        situationBefore = this,
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
      if (capture) then board.taking(orig, dest, taken)
      else board.move(orig, dest)
    after
      .map(_.putOrReplace(Piece(color, promotion), dest))
      .map(board =>
        Move(
          piece = color.pawn,
          orig = orig,
          dest = dest,
          situationBefore = this,
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
      b1    <- board.take(king)
      b2    <- b1.take(rook)
      b3    <- b2.place(color.king, kingTo)
      after <- b3.place(color.rook, rookTo)
    yield after

    val isChess960 =
      if board.variant.standard then false
      else if board.variant.chess960 then true
      else king.file != File.E || !(rook.file == File.A || rook.file == File.H)

    val destInput = if (!isChess960) then List(kingTo, rook) else List(rook)

    for
      a            <- after.toList
      inputKingPos <- destInput
    yield Move(
      piece = color.king,
      orig = king,
      dest = inputKingPos,
      situationBefore = this,
      after = a,
      capture = None,
      castle = Move.Castle((king, kingTo), (rook, rookTo)).some,
      promotion = None,
      enpassant = false
    )

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board init variant, White)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)
