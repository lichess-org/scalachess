package chess

import cats.syntax.all.*

import bitboard.Bitboard
import bitboard.Bitboard.*

import chess.format.Uci
import Square.prevRank
import chess.variant.Crazyhouse
import chess.variant.Antichess

case class Situation(board: Board, color: Color):
  export board.{ history, isOccupied, kingOf, variant }

  lazy val legalMoves = variant.validMoves(this)

  lazy val moves: Map[Square, List[Move]] =
    legalMoves.groupBy(_.orig)

  val movesAt: Square => List[Move] = moves.getOrElse(_, Nil)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Square, List[Square]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Square]] =
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

  def move(from: Square, to: Square, promotion: Option[PromotableRole]): Either[ErrorStr, Move] =
    variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Either[ErrorStr, Move] =
    variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, square: Square): Either[ErrorStr, Drop] =
    variant.drop(this, role, square)

  def withHistory(history: History) =
    copy(board = board withHistory history)

  def withVariant(variant: chess.variant.Variant) =
    copy(board = board withVariant variant)

  def enPassantSquare: Option[Square] =
    potentialEpSquare.flatMap(_ => legalMoves.find(_.enpassant).map(_.dest))

  def unary_! = copy(color = !color)

  // ========================bitboard===========================

  lazy val ourKing   = board.kingPosOf(color)
  lazy val theirKing = board.kingPosOf(!color)
  // alternative version of ourKing is used in Antichess only
  lazy val ourKings: List[Square] = board.kings(color)
  // alternative version of theirKing is used in Antichess only
  lazy val theirKings: List[Square]   = board.kings(!color)
  lazy val us: Bitboard               = board.byColor(color)
  lazy val them: Bitboard             = board.byColor(!color)
  lazy val checkers: Option[Bitboard] = ourKing.map(board.attackers(_, !color))
  val isWhiteTurn: Boolean            = color.white

  def generateMovesAt(square: Square): List[Move] =
    def movesAt =
      val moves = board(square).fold(Nil) { piece =>
        if piece.color != color then Nil
        else
          val targets = ~us
          val bb      = square.bb
          piece.role match
            case Pawn   => genEnPassant(us & bb) ++ genPawn(Bitboard(bb), targets)
            case Knight => genKnight(us & bb, targets)
            case Bishop => genBishop(us & bb, targets)
            case Rook   => genRook(us & bb, targets)
            case Queen  => genQueen(us & bb, targets)
            case King   => genKingAt(targets, square)
      }
      variant.applyVariantEffect(moves).filter(variant.kingSafety)

    // in antichess, if there are capture moves, only capture moves are allowed
    // so, we have to find all captures first,
    // if they're not empty then filter by orig
    // else use the normal moveAt
    if variant.antichess then
      val captureMoves = Antichess.captureMoves(this)
      if captureMoves.nonEmpty then captureMoves.filter(_.orig == square)
      else movesAt
    else movesAt

  def genKingAt(mask: Bitboard, square: Square) =
    val withoutCastles = genUnsafeKing(square, mask)
    if variant.allowsCastling then withoutCastles ::: genCastling(square)
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
  def potentialEpSquare: Option[Square] = history.lastMove.flatMap:
    case Uci.Move(orig, dest, _) =>
      board(dest).flatMap { piece =>
        if piece.color == !color && piece.role == Pawn &&
          orig.yDist(dest) == 2 && orig.rank != piece.color.backRank
        then dest.prevRank(!color)
        else None
      }
    case _ => None

  def genNonKingAndNonPawn(mask: Bitboard): List[Move] =
    genKnight(us & board.knights, mask) ++ genBishop(us & board.bishops, mask) ++
      genRook(us & board.rooks, mask) ++ genQueen(us & board.queens, mask)

  def genNonKing(mask: Bitboard): List[Move] =
    genPawn(us & board.pawns, mask) ++ genNonKingAndNonPawn(mask)

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
      from <- capturers
      to   <- from.pawnAttacks(color) & them & mask
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
      to   <- singleMoves & mask
      from <- Square.at(to.value + (if isWhiteTurn then -8 else 8)).toList
      move <- genPawnMoves(from, to, false)
    yield move

    val s3: List[Move] = for
      to   <- doubleMoves & mask
      from <- Square.at(to.value + (if isWhiteTurn then -16 else 16))
      move <- normalMove(from, to, Pawn, false)
    yield move

    s1 ++ s2 ++ s3

  def genKnight(knights: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- knights
      to   <- Bitboard.knightAttacks(from) & mask
      move <- normalMove(from, to, Knight, isOccupied(to))
    yield move

  def genBishop(bishops: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- bishops
      to   <- from.bishopAttacks(board.occupied) & mask
      move <- normalMove(from, to, Bishop, isOccupied(to))
    yield move

  def genRook(rooks: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- rooks
      to   <- from.rookAttacks(board.occupied) & mask
      move <- normalMove(from, to, Rook, isOccupied(to))
    yield move

  def genQueen(queens: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- queens
      to   <- from.queenAttacks(board.occupied) & mask
      move <- normalMove(from, to, Queen, isOccupied(to))
    yield move

  def genUnsafeKing(king: Square, mask: Bitboard): List[Move] =
    (king.kingAttacks & mask).flatMap(to => normalMove(king, to, King, isOccupied(to)))

  def genSafeKing(mask: Bitboard): List[Move] =
    ourKing.fold(Nil)(king =>
      for
        to <- king.kingAttacks & mask
        if board.attackers(to, !color).isEmpty
        move <- normalMove(king, to, King, isOccupied(to))
      yield move
    )

  def genCastling(king: Square): List[Move] =
    // can castle but which side?
    if !history.castles.can(color) || king.rank != color.backRank then Nil
    else
      val rooks = history.unmovedRooks & Bitboard.rank(color.backRank).value & board.rooks.value
      for
        rook <- rooks
        toKingFile = if rook < king then File.C else File.G
        toRookFile = if rook < king then File.D else File.F
        kingTo     = Square(toKingFile, king.rank)
        rookTo     = Square(toRookFile, rook.rank)
        // calulate different path for standard vs chess960
        path =
          if variant.chess960 || variant.fromPosition
          then Bitboard.between(king, rook) | Bitboard.between(king, kingTo)
          else Bitboard.between(king, rook)
        if (path & board.occupied & ~rook.bb).isEmpty
        kingPath = Bitboard.between(king, kingTo) | king.bb
        if kingPath.forall(variant.castleCheckSafeSquare(board, _, color, board.occupied ^ king.bb))
        if variant.castleCheckSafeSquare(
          board,
          kingTo,
          color,
          board.occupied ^ king.bb ^ rook.bb ^ rookTo.bb
        )
        moves <- castle(king, kingTo, rook, rookTo)
      yield moves

  private def genPawnMoves(from: Square, to: Square, capture: Boolean): List[Move] =
    if from.rank == color.seventhRank then variant.promotableRoles.flatMap(promotion(from, to, _, capture))
    else normalMove(from, to, Pawn, capture).toList

  private def enpassant(orig: Square, dest: Square): Option[Move] =
    val capture = Square(dest.file, orig.rank)
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

  private def normalMove(orig: Square, dest: Square, role: Role, capture: Boolean): Option[Move] =
    val taken = if capture then Option(dest) else None
    val after =
      if capture then board.taking(orig, dest, taken)
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

  private def promotion(
      orig: Square,
      dest: Square,
      promotion: PromotableRole,
      capture: Boolean
  ): Option[Move] =
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
  private def castle(king: Square, kingTo: Square, rook: Square, rookTo: Square): List[Move] =
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
      a               <- after.toList
      inputKingSquare <- destInput
    yield Move(
      piece = color.king,
      orig = king,
      dest = inputKingSquare,
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
