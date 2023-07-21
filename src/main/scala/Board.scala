package chess

import variant.{ Crazyhouse, Variant }
import bitboard.Board as BBoard
import bitboard.Bitboard

case class Board(
    board: BBoard,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None
):

  export history.{ castles, unmovedRooks }
  export board.{
    attackers,
    bishops,
    black,
    byColor,
    byPiece,
    byRole,
    byRoleOf,
    colorAt,
    fold,
    foreach,
    isCheck,
    isOccupied,
    kingOf,
    kingPosOf,
    kings,
    knights,
    nbPieces,
    occupied,
    pawns,
    pieceMap as pieces,
    pieces as allPieces,
    piecesOf,
    queens,
    rooks,
    sliderBlockers,
    sliders,
    white
  }

  inline def apply(inline color: Color): Bitboard = color.fold(white, black)
  inline def apply(inline color: Color, inline role: Role): Bitboard =
    color.fold(white, black) & board.byRole(role)

  inline def apply(inline at: Square): Option[Piece]     = board.pieceAt(at)
  inline def apply(inline file: File, inline rank: Rank) = board.pieceAt(Square(file, rank))

  def checkColor: Option[Color] = checkWhite.yes.option(White) orElse checkBlack.yes.option(Black)

  lazy val checkWhite: Check = checkOf(White)
  lazy val checkBlack: Check = checkOf(Black)

  def checkOf(c: Color): Check = variant.kingThreatened(this, c)

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

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(board = BBoard.fromMap(newPieces))

  def withVariant(v: Variant): Board =
    if v == Crazyhouse then copy(variant = v).ensureCrazyData
    else copy(variant = v)

  def withCrazyData(data: Crazyhouse.Data)         = copy(crazyData = Option(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board =
    withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData: Board = withCrazyData(crazyData | Crazyhouse.Data.init)

  inline def updateHistory(inline f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = board.piece(p).count
  def count(c: Color): Int = board.color(c).count

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  inline def situationOf(inline color: Color) = Situation(this, color)

  def materialImbalance: Int = variant.materialImbalance(this)

  def kingsAndBishopsOnly: Boolean =
    (kings | bishops) == occupied

  def kingsAndKnightsOnly: Boolean =
    (kings | knights) == occupied

  def onlyKnights: Boolean = knights == occupied

  def minors: Bitboard =
    bishops | knights

  def kingsAndMinorsOnly: Boolean =
    (kings | minors) == occupied

  def kingsRooksAndMinorsOnly: Boolean =
    (kings | rooks | minors) == occupied

  def kingsAndBishopsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings | bishops)

  def kingsAndMinorsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings | minors)

  def kingsOnly = kings == occupied

  def kingsOnlyOf(color: Color) =
    onlyOf(color, kings)

  def kingsAndKnightsOnlyOf(color: Color) =
    onlyOf(color, kings | knights)

  def onlyOf(color: Color, roles: Bitboard): Boolean =
    val colorPieces = byColor(color)
    (roles & colorPieces) == colorPieces

  def nonKingsOf(color: Color): Bitboard =
    apply(color) & ~kings

  def nonKing: Bitboard =
    occupied & ~kings

  override def toString = s"$board $variant ${history.lastMove}\n"

object Board:

  def apply(pieces: PieceMap, history: History, variant: Variant, crazyData: Option[Crazyhouse.Data]): Board =
    Board(BBoard.fromMap(pieces), history, variant, crazyData)

  def apply(pieces: Iterable[(Square, Piece)], variant: Variant): Board =
    Board(pieces, variant.castles, variant)

  def apply(pieces: Iterable[(Square, Piece)], castles: Castles, variant: Variant): Board =
    val board        = BBoard.fromMap(pieces.toMap)
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Board(board, History(castles = castles, unmovedRooks = unmovedRooks), variant, variantCrazyData(variant))

  def apply(board: BBoard, variant: Variant): Board =
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Board(
      board,
      History(castles = variant.castles, unmovedRooks = unmovedRooks),
      variant,
      variantCrazyData(variant)
    )

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    variant.crazyhouse option Crazyhouse.Data.init
