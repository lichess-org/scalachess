package chess

import variant.{ Crazyhouse, Variant }
import bitboard.Board as BBoard
import Castles.*

case class Board(
    board: BBoard,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None
):

  inline def apply(inline at: Pos): Option[Piece]        = board.pieceAt(at)
  inline def apply(inline file: File, inline rank: Rank) = board.pieceAt(Pos(file, rank))

  // todo remove
  lazy val pieces = board.pieceMap
  // todo maybe remove?
  lazy val allPieces = board.pieces

  def contains = board.contains

  // TODO fix
  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] =
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)

  def rolesOf(c: Color): List[Role] =
    allPieces.collect { case p if p.color == c => p.role }

  // todo fix
  inline def actorAt(inline at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = board.piecesOf(c)

  def kingPosOf(c: Color): Option[Pos] = board.king(c)

  def check(c: Color): Boolean = c.fold(checkWhite, checkBlack)

  def checkColor: Option[Color] = checkWhite.option(White) orElse checkBlack.option(Black)

  lazy val checkWhite: Boolean = checkOf(White)
  lazy val checkBlack: Boolean = checkOf(Black)

  // todo fix
  private def checkOf(c: Color): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(this, !c, kingPos)
    }

  // todo fix
  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from).map(_.destinations)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def withBoard(b: BBoard): Board = copy(board = b)

  def place(piece: Piece, at: Pos): Option[Board] =
    board.put(piece, at).map(withBoard)

  def take(at: Pos): Option[Board] =
    board.take(at).map(withBoard)

  def move(orig: Pos, dest: Pos): Option[Board] =
    board.move(orig, dest).map(withBoard)

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    board.taking(orig, dest, taking).map(withBoard)

  lazy val occupation: Color.Map[Set[Pos]] = board.occupation

  inline def hasPiece(inline p: Piece) = board.hasPiece(p)

  def promote(pos: Pos): Option[Board] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    } yield b3

  def castles: Castles = history.castles

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

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.value.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && piece.color.backRank == pos.rank)
      )
    }

  inline def updateHistory(inline f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = board.piece(p).count
  def count(c: Color): Int = board.color(c).count

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  inline def situationOf(inline color: Color) = Situation(this, color)

  inline def valid(inline strict: Boolean): Boolean = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$board $variant ${history.lastMove}\n"

object Board:

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Iterable[(Pos, Piece)], castles: Castles, variant: Variant): Board =
    Board(BBoard.fromMap(pieces.toMap), History(castles = castles), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init
