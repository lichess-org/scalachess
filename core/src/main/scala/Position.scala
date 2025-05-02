package chess

import cats.syntax.all.*
import chess.format.Uci

import variant.*

case class Position(board: Board, history: History, variant: Variant, color: Color):

  export history.{ castles, unmovedRooks, crazyData }
  // format: off
  export board.{ attackers, attacks, bishops, black, byColor, byPiece, byRole, byRoleOf, colorAt,
    fold, foreach, isCheck, isOccupied, kingOf, kingPosOf, kings, kingsAndBishopsOnly,
    kingsAndBishopsOnlyOf, kingsAndKnightsOnly, kingsAndKnightsOnlyOf, kingsAndMinorsOnly,
    kingsOnly, kingsOnlyOf, kingsRooksAndMinorsOnly, knights, nbPieces, nonKingsOf, occupied,
    onlyKnights, onlyOf, pawns, piece, pieceAt, pieceMap as pieces, pieces as allPieces, piecesOf,
    queens, rooks, sliderBlockers, sliders, white, count
  }
  // format: on

  export color.white as isWhiteTurn

  def withCastles(c: Castles) = updateHistory(_.withCastles(c))

  def unary_! : Position = withColor(color = !color)

  def withPieces(newPieces: PieceMap) = copy(board = Board.fromMap(newPieces))

  def withVariant(v: Variant): Position =
    if v == Crazyhouse then copy(variant = v).ensureCrazyData
    else copy(variant = v)

  def withCrazyData(data: Crazyhouse.Data): Position         = updateHistory(_.copy(crazyData = data.some))
  def withCrazyData(data: Option[Crazyhouse.Data]): Position = updateHistory(_.copy(crazyData = data))
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Position =
    withCrazyData(f(crazyData.getOrElse(Crazyhouse.Data.init)))

  def ensureCrazyData: Position = withCrazyData(crazyData.getOrElse(Crazyhouse.Data.init))

  inline def updateHistory(inline f: History => History): Position = copy(history = f(history))

  def withBoard(b: Board): Position = copy(board = b)

  def withColor(color: Color): Position = copy(color = color)

  def materialImbalance: Int = variant.materialImbalance(board)

  override def toString = s"$board $variant ${history.lastMove} $color"

  lazy val moves: Map[Square, List[Move]] =
    legalMoves.groupBy(_.orig)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Square, Bitboard] = legalMoves.groupMapReduce(_.orig)(_.dest.bb)(_ | _)

  def drops: Option[List[Square]] =
    variant match
      case v: Crazyhouse.type => v.possibleDrops(this)
      case _                  => None

  def checkSquare: Option[Square] = if check.yes then ourKing else None

  def move(from: Square, to: Square, promotion: Option[PromotableRole]): Either[ErrorStr, Move] =
    variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Either[ErrorStr, Move] =
    variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, square: Square): Either[ErrorStr, Drop] =
    variant.drop(this, role, square)

  def playable(strict: Boolean): Boolean =
    variant.valid(this, strict) && !end && copy(color = !color).check.no

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def checkMate: Boolean = variant.checkmate(this)

  inline def staleMate: Boolean = variant.staleMate(this)

  inline def autoDraw: Boolean = variant.autoDraw(this) || variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = history.threefoldRepetition

  inline def variantEnd: Boolean = variant.specialEnd(this)

  lazy val check: Check               = checkOf(color)
  inline def checkOf(c: Color): Check = variant.kingThreatened(board, c)

  lazy val status: Option[Status] =
    if checkMate then Status.Mate.some
    else if variantEnd then Status.VariantEnd.some
    else if staleMate then Status.Stalemate.some
    else if autoDraw then Status.Draw.some
    else none

  inline def winner: Option[Color] = variant.winner(this)

  lazy val legalMoves: List[Move] = variant.validMoves(this)

  lazy val enPassantSquare: Option[Square] =
    potentialEpSquare >> legalMoves.find(_.enpassant).map(_.dest)

  lazy val ourKing: Option[Square]   = kingPosOf(color)
  lazy val theirKing: Option[Square] = kingPosOf(!color)
  // alternative version of ourKing is used in Antichess only
  lazy val ourKings: List[Square] = kings(color)
  // alternative version of theirKing is used in Antichess only
  lazy val theirKings: List[Square] = kings(!color)
  lazy val us: Bitboard             = byColor(color)
  lazy val them: Bitboard           = byColor(!color)
  lazy val checkers: Bitboard       = ourKing.fold(Bitboard.empty)(board.attackers(_, !color))

  def generateMovesAt(square: Square): List[Move] =
    def movesAt =
      val moves = board.pieceAt(square).fold(Nil) { piece =>
        if piece.color != color then Nil
        else
          val targets = ~us
          val bb      = square.bb
          piece.role match
            case Pawn   => genEnPassant(us & bb) ++ genPawn(bb, targets)
            case Knight => genKnight(us & bb, targets)
            case Bishop => genBishop(us & bb, targets)
            case Rook   => genRook(us & bb, targets)
            case Queen  => genQueen(us & bb, targets)
            case King   => genKingAt(targets, square)
      }

      if variant.atomic then moves.map(Atomic.explodeSurroundingPieces).filter(variant.kingSafety)
      else moves.filter(variant.kingSafety)

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
  lazy val potentialEpSquare: Option[Square] =
    history.lastMove.flatMap:
      case Uci.Move(orig, dest, _) =>
        board
          .pieceAt(dest)
          .flatMap: piece =>
            if piece.color != color && piece.role == Pawn &&
              orig.yDist(dest) == 2 && orig.rank != piece.color.backRank
            then dest.prevRank(!color)
            else None
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
      from <- Square(to.value + (if isWhiteTurn then -8 else 8)).toList
      move <- genPawnMoves(from, to, false)
    yield move

    val s3: List[Move] = for
      to   <- doubleMoves & mask
      from <- Square(to.value + (if isWhiteTurn then -16 else 16))
      move <- normalMove(from, to, Pawn, false)
    yield move

    s1 ++ s2 ++ s3

  def genKnight(knights: Bitboard, mask: Bitboard): List[Move] =
    for
      from <- knights
      to   <- from.knightAttacks & mask
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
    ourKing.fold(Nil)(genSafeKing(_, mask))

  def genSafeKing(king: Square, mask: Bitboard): List[Move] =
    for
      to <- king.kingAttacks & mask
      if board.attackers(to, !color).isEmpty
      move <- normalMove(king, to, King, isOccupied(to))
    yield move

  def genCastling(king: Square): List[Move] =
    if !history.castles.can(color) || king.rank != color.backRank then Nil
    else
      val rooks = Bitboard.rank(color.backRank) & board.rooks & history.unmovedRooks.value
      for
        rook <- rooks
        if (rook.value < king.value && history.castles.can(color, QueenSide))
          || (rook.value > king.value && history.castles.can(color, KingSide))
        toKingFile = if rook.value < king.value then File.C else File.G
        toRookFile = if rook.value < king.value then File.D else File.F
        kingTo     = Square(toKingFile, king.rank)
        rookTo     = Square(toRookFile, rook.rank)
        // calulate different path for standard vs chess960
        path =
          if variant.chess960 || variant.fromPosition
          then Bitboard.between(king, rook) | Bitboard.between(king, kingTo)
          else Bitboard.between(king, rook)
        if (path & board.occupied & ~rook.bl).isEmpty
        kingPath = Bitboard.between(king, kingTo) | king.bl
        if kingPath.forall(variant.castleCheckSafeSquare(board, _, color, board.occupied ^ king.bl))
        if variant.castleCheckSafeSquare(
          board,
          kingTo,
          color,
          board.occupied ^ king.bl ^ rook.bl ^ rookTo.bl
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
          boardBefore = this,
          after = withBoard(after),
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
        boardBefore = this,
        after = withBoard(board),
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
          boardBefore = this,
          after = withBoard(board),
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

    val boardAfter = for
      b1    <- board.take(king)
      b2    <- b1.take(rook)
      b3    <- b2.put(color.king, kingTo)
      after <- b3.put(color.rook, rookTo)
    yield after

    val isChess960 =
      if variant.standard then false
      else if variant.chess960 then true
      else king.file != File.E || !(rook.file == File.A || rook.file == File.H)

    val destInput = if !isChess960 then List(rook, kingTo) else List(rook)

    for
      after           <- boardAfter.map(withBoard).toList
      inputKingSquare <- destInput
    yield Move(
      piece = color.king,
      orig = king,
      dest = inputKingSquare,
      boardBefore = this,
      after = after,
      capture = None,
      castle = Move.Castle(king, kingTo, rook, rookTo).some,
      promotion = None,
      enpassant = false
    )

object Position:

  case class AndFullMoveNumber(board: Position, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(board.color)

  def apply(
      pieces: PieceMap,
      history: History,
      variant: Variant,
      crazyData: Option[Crazyhouse.Data],
      color: Option[Color]
  ) =
    new Position(Board.fromMap(pieces), history.copy(crazyData = crazyData), variant, color.getOrElse(White))

  def apply(board: Board, variant: Variant, color: Option[Color]): Position =
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Position(
      board,
      History(
        castles = variant.castles,
        unmovedRooks = unmovedRooks,
        crazyData = variant.crazyhouse.option(Crazyhouse.Data.init)
      ),
      variant,
      color.getOrElse(White)
    )

  def apply(pieces: Iterable[(Square, Piece)], variant: Variant, color: Option[Color]): Position =
    Position(pieces, variant.castles, variant, color)

  def apply(
      pieces: Iterable[(Square, Piece)],
      castles: Castles,
      variant: Variant,
      color: Option[Color]
  ): Position =
    val board        = Board.fromMap(pieces.toMap)
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Position(
      board,
      History(
        castles = variant.castles,
        unmovedRooks = unmovedRooks,
        crazyData = variant.crazyhouse.option(Crazyhouse.Data.init)
      ),
      variant,
      color.getOrElse(White)
    )

  def apply(variant: chess.variant.Variant): Position = Position.init(variant, White)
  def init(variant: Variant, color: Color): Position =
    Position(Board.fromMap(variant.pieces), variant, color.some)
