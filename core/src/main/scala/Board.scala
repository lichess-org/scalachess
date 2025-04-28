package chess

import cats.syntax.all.*

import variant.{ Crazyhouse, Variant }
import bitboard.Board as BBoard
import bitboard.Bitboard

case class Board(board: BBoard, history: History, variant: Variant, color: Color):

  export history.{ castles, unmovedRooks, crazyData }
  // format: off
  export board.{ attackers, bishops, black, byColor, byPiece, byRole, byRoleOf, colorAt,
    fold, foreach, isCheck, isOccupied, kingOf, kingPosOf, kings, kingsAndBishopsOnly,
    kingsAndBishopsOnlyOf, kingsAndKnightsOnly, kingsAndKnightsOnlyOf, kingsAndMinorsOnly,
    kingsOnly, kingsOnlyOf, kingsRooksAndMinorsOnly, knights, nbPieces, nonKingsOf, occupied,
    onlyKnights, onlyOf, pawns, piece, pieceAt, pieceMap as pieces, pieces as allPieces, piecesOf,
    queens, rooks, sliderBlockers, sliders, white, apply, count
  }
  // format: on

  def toSituation: Situation = Situation(this, color)

  def withBoard(b: BBoard): Board = copy(board = b)

  def place(piece: Piece, at: Square): Option[Board] =
    board.put(piece, at).map(withBoard)

  def putOrReplace(piece: Piece, at: Square): Board =
    withBoard(board.putOrReplace(piece, at))

  def take(at: Square): Option[Board] =
    board.take(at).map(withBoard)

  def move(orig: Square, dest: Square): Option[Board] =
    board.move(orig, dest).map(withBoard)

  def taking(orig: Square, dest: Square, taking: Option[Square] = None): Option[Board] =
    board.taking(orig, dest, taking).map(withBoard)

  def promote(orig: Square, dest: Square, piece: Piece): Option[Board] =
    board.promote(orig, dest, piece).map(withBoard)

  def withCastles(c: Castles) = updateHistory(_.withCastles(c))

  def withPieces(newPieces: PieceMap) = copy(board = BBoard.fromMap(newPieces))

  def withVariant(v: Variant): Board =
    if v == Crazyhouse then copy(variant = v).ensureCrazyData
    else copy(variant = v)

  def withCrazyData(data: Crazyhouse.Data): Board         = updateHistory(_.copy(crazyData = data.some))
  def withCrazyData(data: Option[Crazyhouse.Data]): Board = updateHistory(_.copy(crazyData = data))
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board =
    withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData: Board = withCrazyData(crazyData | Crazyhouse.Data.init)

  inline def updateHistory(inline f: History => History) = copy(history = f(history))

  def autoDraw: Boolean = variant.autoDraw(this)

  inline def situationOf(inline color: Color) = Situation(this, color)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$board $variant ${history.lastMove}\n"

object Board:

  def apply(
      pieces: PieceMap,
      history: History,
      variant: Variant,
      crazyData: Option[Crazyhouse.Data],
      color: Option[Color]
  ) =
    new Board(BBoard.fromMap(pieces), history.copy(crazyData = crazyData), variant, color.getOrElse(White))

  def apply(board: BBoard, variant: Variant, color: Option[Color]): Board =
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Board(
      board,
      History(
        castles = variant.castles,
        unmovedRooks = unmovedRooks,
        crazyData = variant.crazyhouse.option(Crazyhouse.Data.init)
      ),
      variant,
      color.getOrElse(White)
    )

  def apply(pieces: Iterable[(Square, Piece)], variant: Variant, color: Option[Color]): Board =
    Board(pieces, variant.castles, variant, color)

  def apply(
      pieces: Iterable[(Square, Piece)],
      castles: Castles,
      variant: Variant,
      color: Option[Color]
  ): Board =
    val board        = BBoard.fromMap(pieces.toMap)
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Board(
      board,
      History(
        castles = variant.castles,
        unmovedRooks = unmovedRooks,
        crazyData = variant.crazyhouse.option(Crazyhouse.Data.init)
      ),
      variant,
      color.getOrElse(White)
    )

  def init(variant: Variant): Board = Board(BBoard.fromMap(variant.pieces), variant, White.some)
