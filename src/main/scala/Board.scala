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

  export board.{
    attackers,
    bishops,
    black,
    isCheck,
    kings,
    knights,
    nbPieces,
    occupied,
    pawns,
    queens,
    rooks,
    sliders,
    white
  }

  inline def apply(inline color: Color): Bitboard = color.fold(white, black)
  inline def apply(inline color: Color, inline role: Role): Bitboard =
    color.fold(white, black) & board.byRole(role)

  inline def apply(inline at: Pos): Option[Piece]        = board.pieceAt(at)
  inline def apply(inline file: File, inline rank: Rank) = board.pieceAt(Pos(file, rank))

  // todo remove
  lazy val pieces = board.pieceMap
  // todo maybe remove?
  lazy val allPieces = board.pieces

  def piecesOf(c: Color): Map[Pos, Piece] = board.piecesOf(c)

  def kingPosOf(c: Color): Bitboard = board.kings & board.byColor(c)

  def checkColor: Option[Color] = checkWhite.yes.option(White) orElse checkBlack.yes.option(Black)

  lazy val checkWhite: Check = checkOf(White)
  lazy val checkBlack: Check = checkOf(Black)

  def checkOf(c: Color): Check = variant.kingThreatened(this, c)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def withBoard(b: BBoard): Board = copy(board = b)

  def place(piece: Piece, at: Pos): Option[Board] =
    board.put(piece, at).map(withBoard)

  def putOrReplace(piece: Piece, at: Pos): Board =
    withBoard(board.putOrReplace(piece, at))

  def take(at: Pos): Option[Board] =
    board.take(at).map(withBoard)

  def move(orig: Pos, dest: Pos): Option[Board] =
    board.move(orig, dest).map(withBoard)

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    board.taking(orig, dest, taking).map(withBoard)

  lazy val occupation: Color.Map[Set[Pos]] = board.occupation

  inline def hasPiece(inline p: Piece) = board.hasPiece(p)

  def promote(pos: Pos): Option[Board] =
    for
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    yield b3

  export history.{ castles, unmovedRooks }

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(board = BBoard.fromMap(newPieces))

  def withVariant(v: Variant): Board =
    if (v == Crazyhouse)
      copy(variant = v).ensureCrazyData
    else
      copy(variant = v)

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

  inline def valid(inline strict: Boolean): Boolean = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  lazy val kingsAndBishopsOnly: Boolean =
    (kings | bishops) == occupied

  lazy val kingsAndKnightsOnly: Boolean =
    (kings | knights) == occupied

  lazy val onlyKnights: Boolean = knights == occupied

  lazy val minors: Bitboard =
    bishops | knights

  lazy val kingsAndMinorsOnly: Boolean =
    (kings | minors) == occupied

  lazy val kingsRooksAndMinorsOnly: Boolean =
    (kings | rooks | minors) == occupied

  def kingsAndBishopsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings | bishops)

  def kingsAndMinorsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings | minors)

  def kingsOnly =
    kings == occupied

  def kingsOnlyOf(color: Color) =
    onlyOf(color, kings)

  def kingsAndKnightsOnlyOf(color: Color) =
    onlyOf(color, kings | knights)

  def onlyOf(color: Color, roles: Bitboard): Boolean =
    val colorPieces = apply(color)
    (roles & colorPieces) == colorPieces

  def nonKingsOf(color: Color): Bitboard =
    apply(color) & ~kings

  lazy val nonKing: Bitboard =
    occupied & ~kings

  override def toString = s"$board $variant ${history.lastMove}\n"

object Board:

  def apply(pieces: PieceMap, history: History, variant: Variant, crazyData: Option[Crazyhouse.Data]): Board =
    Board(BBoard.fromMap(pieces), history, variant, crazyData)

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces, variant.castles, variant)

  def apply(pieces: Iterable[(Pos, Piece)], castles: Castles, variant: Variant): Board =
    val board        = BBoard.fromMap(pieces.toMap)
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Board(board, History(castles = castles, unmovedRooks = unmovedRooks), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    variant.crazyhouse option Crazyhouse.Data.init
