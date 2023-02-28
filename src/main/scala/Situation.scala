package chess

import scala.collection.mutable.ListBuffer
import cats.data.Validated
import cats.syntax.all.*

import bitboard.Bitboard
import bitboard.Bitboard.*

import chess.format.Uci
import chess.variant.Chess960
import Pos.prevRank
import chess.variant.Crazyhouse
import chess.variant.Antichess

case class Situation(board: Board, color: Color):
  export board.{ history, isOccupied, kingOf, variant }

  lazy val legalMoves = variant.validMoves(this)

  lazy val moves: Map[Pos, List[Move]] =
    legalMoves.groupBy(_.orig)

  val movesAt: Pos => List[Move] = moves.getOrElse(_, Nil)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    variant match
      case v: Crazyhouse.type => v possibleDrops this
      case _                  => None

  lazy val check: Check = board checkOf color

  def checkSquare = if check.yes then ourKing else None

  inline def checkMate: Boolean = variant checkmate this

  inline def staleMate: Boolean = variant staleMate this

  inline def autoDraw: Boolean = board.autoDraw || variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = history.threefoldRepetition

  inline def variantEnd = variant specialEnd this

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def winner: Option[Color] = variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && copy(color = !color).check.no

  lazy val status: Option[Status] =
    if checkMate then Status.Mate.some
    else if variantEnd then Status.VariantEnd.some
    else if staleMate then Status.Stalemate.some
    else if autoDraw then Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[ErrorStr, Move] =
    variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[ErrorStr, Move] =
    variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[ErrorStr, Drop] =
    variant.drop(this, role, pos)

  def withHistory(history: History) =
    copy(board = board withHistory history)

  def withVariant(variant: chess.variant.Variant) =
    copy(board = board withVariant variant)

  def enPassantSquare: Option[Pos] =
    potentialEpSquare.flatMap(_ => legalMoves.find(_.enpassant).map(_.dest))

  def unary_! = copy(color = !color)

  // ========================bitboard===========================

  lazy val ourKing   = board.kingPosOf(color)
  lazy val theirKing = board.kingPosOf(!color)
  // alternative version of ourKing is used in Antichess only
  lazy val ourKings: List[Pos] = board.kings(color)
  // alternative version of theirKing is used in Antichess only
  lazy val theirKings: List[Pos]      = board.kings(!color)
  lazy val us: Bitboard               = board.byColor(color)
  lazy val them: Bitboard             = board.byColor(!color)
  lazy val checkers: Option[Bitboard] = ourKing.map(board.attackers(_, !color))
  lazy val sliderBlockers: Bitboard   = board.sliderBlockers(color)
  val isWhiteTurn: Boolean            = color.white

  // TODO test generateMovesAt(pos) = generateMoves.filter(_.orig == pos)
  // TODO test generateMoves == generateMovesAt(pos) for all pos
  def generateMovesAt(pos: Pos): List[Move] =
    def movesAt =
      val moves = board.apply(pos).fold(Nil) { piece =>
        if piece.color != color then Nil
        else
          val targets = ~us
          val bb      = pos.bb
          piece.role match
            case Pawn   => genEnPassant(us & bb) ++ genPawn(bb, targets)
            case Knight => genKnight(us & bb, targets)
            case Bishop => genBishop(us & bb, targets)
            case Rook   => genRook(us & bb, targets)
            case Queen  => genQueen(us & bb, targets)
            case King   => genKingAt(targets, pos)
      }
      variant.applyVariantEffect(moves).filter(variant.kingSafety)

    // in antichess, if there are capture moves, only capture moves are allowed
    // so, we have to find all captures first,
    // if they're not empty then filter by orig
    // else use the normal moveAt
    if variant.antichess then
      val captureMoves = Antichess.captureMoves(this)
      if captureMoves.nonEmpty then captureMoves.filter(_.orig == pos)
      else movesAt
    else movesAt

  def genKingAt(mask: Bitboard, pos: Pos) =
    val withoutCastles = genUnsafeKing(pos, mask)
    if variant.allowsCastling then withoutCastles ::: genCastling(pos)
    else withoutCastles

  def genEnPassant(pawns: Bitboard): List[Move] =
    potentialEpSquare.fold(Nil)(ep =>
      val pawnsCanEnPassant = pawns & ep.pawnAttacks(!color)
      pawnsCanEnPassant.flatMap(enpassant(_, ep))
    )

  /** Get the potential en passant square, if any.
    * In order to be a potential en passant square,
    * the last move must have been a double pawn push
    * and not start from the back rank
    */
  def potentialEpSquare: Option[Pos] = history.lastMove.flatMap {
    case Uci.Move(orig, dest, _) =>
      board(dest).flatMap { piece =>
        if piece.color == !color && piece.role == Pawn &&
          orig.yDist(dest) == 2 && orig.rank != piece.color.backRank
        then dest.prevRank(!color)
        else None
      }
    case _ => None
  }

  def genNonKing(mask: Bitboard): List[Move] =
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
  def genPawn(pawns: Bitboard, mask: Bitboard): List[Move] =
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
      if isWhiteTurn then (board.white & pawns) << 8
      else (board.black & pawns) >>> 8
    }

    val doubleMoves =
      ~board.occupied &
        (if isWhiteTurn then singleMoves << 8 else singleMoves >>> 8) &
        (if variant.horde then Bitboard.rank(color.fourthRank) | Bitboard.rank(color.thirdRank)
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

  def genKnight(knights: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- knights.occupiedSquares
      targets = Bitboard.knightAttacks(from) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Knight, isOccupied(to))
    yield move

  def genBishop(bishops: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- bishops.occupiedSquares
      targets = from.bishopAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Bishop, isOccupied(to))
    yield move

  def genRook(rooks: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- rooks.occupiedSquares
      targets = from.rookAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Rook, isOccupied(to))
    yield move

  def genQueen(queens: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- queens.occupiedSquares
      targets = from.queenAttacks(board.occupied) & mask
      to   <- targets.occupiedSquares
      move <- normalMove(from, to, Queen, isOccupied(to))
    yield move

  def genUnsafeKing(king: Pos, mask: Bitboard): List[Move] =
    val targets = king.kingAttacks & mask
    targets.occupiedSquares.flatMap(to => normalMove(king, to, King, isOccupied(to)))

  def genSafeKing(mask: Bitboard): List[Move] =
    ourKing.fold(Nil)(king =>
      val targets = king.kingAttacks & mask
      for
        to <- targets.occupiedSquares
        if board.board.attackers(to, !color).isEmpty
        move <- normalMove(king, to, King, isOccupied(to))
      yield move
    )

  def genCastling(king: Pos): List[Move] =
    // can castle but which side?
    if !history.castles.can(color) || king.rank != color.backRank then Nil
    else
      val rooks = history.unmovedRooks & Bitboard.rank(color.backRank) & board.rooks
      for
        rook <- rooks.occupiedSquares
        toKingFile = if rook < king then File.C else File.G
        toRookFile = if rook < king then File.D else File.F
        kingTo     = Pos(toKingFile, king.rank)
        rookTo     = Pos(toRookFile, rook.rank)
        // calulate different path for standard vs chess960
        path =
          if variant.chess960 || variant.fromPosition
          then Bitboard.between(king, rook) | Bitboard.between(king, kingTo)
          else Bitboard.between(king, rook)
        if (path & board.occupied & ~rook.bb).isEmpty
        kingPath = Bitboard.between(king, kingTo) | king.bb
        if kingPath.occupiedSquares
          .forall(variant.castleCheckSafeSquare(board, _, color, board.occupied ^ king.bb))
        if variant.castleCheckSafeSquare(
          board,
          kingTo,
          color,
          board.occupied ^ king.bb ^ rook.bb ^ rookTo.bb
        )
        moves <- castle(king, kingTo, rook, rookTo)
      yield moves

  private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
    if from.rank == color.seventhRank then variant.promotableRoles.flatMap(promotion(from, to, _, capture))
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
    board
      .promote(orig, dest, color - promotion)
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
  // if the variant is not either of those two then
  //     if King and Rook are in standard position  => 2 moves
  //     else => 1 move
  // check logic in isChess960 function
  // make sure that the 960 move is first since it will be the representative
  // move and we want 960 uci notation
  private def castle(king: Pos, kingTo: Pos, rook: Pos, rookTo: Pos): List[Move] =
    val after = for
      b1    <- board.take(king)
      b2    <- b1.take(rook)
      b3    <- b2.place(color.king, kingTo)
      after <- b3.place(color.rook, rookTo)
    yield after

    val isChess960 =
      if variant.standard then false
      else if variant.chess960 then true
      else king.file != File.E || !(rook.file == File.A || rook.file == File.H)

    val destInput = if !isChess960 then List(rook, kingTo) else List(rook)

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
