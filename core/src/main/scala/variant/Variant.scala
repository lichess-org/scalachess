package chess
package variant

import cats.Eq
import cats.syntax.all.*
import chess.bitboard.{ Bitboard, Board as BBoard }
import chess.format.Fen

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Variant.Id,
    val key: Variant.LilaKey,
    val uciKey: Variant.UciKey,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean
):

  def pieces: Map[Square, Piece]

  inline def standard: Boolean      = this == Standard
  inline def chess960: Boolean      = this == Chess960
  inline def fromPosition: Boolean  = this == FromPosition
  inline def kingOfTheHill: Boolean = this == KingOfTheHill
  inline def threeCheck: Boolean    = this == ThreeCheck
  inline def antichess: Boolean     = this == Antichess
  inline def atomic: Boolean        = this == Atomic
  inline def horde: Boolean         = this == Horde
  inline def racingKings: Boolean   = this == RacingKings
  inline def crazyhouse: Boolean    = this == Crazyhouse

  inline def exotic: Boolean = !standard

  def allowsCastling: Boolean = !castles.isEmpty

  def makeUnmovedRooks(rooks: Bitboard): UnmovedRooks =
    if allowsCastling then UnmovedRooks(rooks) else UnmovedRooks.none

  protected val backRank: Vector[Role] =
    Vector(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

  def castles: Castles = Castles.init

  val initialFen: Fen.Full = Fen.Full.initial

  def isValidPromotion(promotion: Option[PromotableRole]): Boolean =
    promotion match
      case None                                 => true
      case Some(Queen | Rook | Knight | Bishop) => true
      case _                                    => false

  def validMoves(board: Board): List[Move]

  def pieceThreatened(board: Board, by: Color, to: Square): Boolean =
    board.attacks(to, by)

  def kingThreatened(board: BBoard, color: Color): Check =
    board.isCheck(color)

  def checkWhite(board: BBoard): Check = kingThreatened(board, White)
  def checkBlack(board: BBoard): Check = kingThreatened(board, Black)

  def checkColor(board: BBoard): Option[Color] =
    checkWhite(board).yes.option(White).orElse(checkBlack(board).yes.option(Black))

  def kingSafety(m: Move): Boolean =
    kingThreatened(m.after.board, m.color).no

  def castleCheckSafeSquare(board: Board, kingTo: Square, color: Color, occupied: Bitboard): Boolean =
    board.attackers(kingTo, !color, occupied).isEmpty

  def move(
      board: Board,
      from: Square,
      to: Square,
      promotion: Option[PromotableRole]
  ): Either[ErrorStr, Move] =
    // Find the move in the variant specific list of valid moves
    def findMove(from: Square, to: Square) =
      board.generateMovesAt(from).find(_.dest == to)

    for
      piece <- board(from).toRight(ErrorStr(s"No piece on ${from.key}"))
      _     <- Either.cond(piece.color == board.color, piece, ErrorStr(s"Not my piece on ${from.key}"))
      m1    <- findMove(from, to).toRight(ErrorStr(s"Piece on ${from.key} cannot move to ${to.key}"))
      m2 <- m1
        .withPromotion(promotion)
        .toRight(ErrorStr(s"Piece on ${from.key} cannot promote to $promotion"))
      m3 <- Either.cond(
        isValidPromotion(promotion),
        m2,
        ErrorStr(s"Cannot promote to $promotion in this game mode")
      )
    yield m3

  def drop(board: Board, role: Role, square: Square): Either[ErrorStr, Drop] =
    ErrorStr(s"$this variant cannot drop $role $square").asLeft

  def staleMate(board: Board): Boolean = board.check.no && board.legalMoves.isEmpty

  def checkmate(board: Board): Boolean = board.check.yes && board.legalMoves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(board: Board): Option[Color] =
    if board.checkMate || specialEnd(board) then Option(!board.color) else None

  def specialEnd(board: Board): Boolean = false

  def specialDraw(board: Board): Boolean = false

  def autoDraw(board: Board): Boolean =
    isInsufficientMaterial(board) || fiftyMoves(board.history) || board.history.fivefoldRepetition

  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: Board): Int =
    board.fold(0): (acc, color, role) =>
      Role
        .valueOf(role)
        .fold(acc): value =>
          acc + value * color.fold(1, -1)

  /** Returns true if neither player can win. The game should end immediately.
    */
  def isInsufficientMaterial(board: Board): Boolean = InsufficientMatingMaterial(board)

  /** Returns true if the other player cannot win. This is relevant when the
    * side to move times out or disconnects. Instead of losing on time,
    * the game should be drawn.
    */
  def opponentHasInsufficientMaterial(board: Board): Boolean =
    InsufficientMatingMaterial(board, !board.color)

  def fiftyMoves(history: History): Boolean = history.halfMoveClock >= HalfMoveClock(100)

  def isIrreversible(move: Move): Boolean =
    (move.piece.is(Pawn)) || move.captures || move.promotes || move.castles

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board = board

  protected def pawnsOnPromotionRank(board: Board, color: Color): Boolean =
    board(color, Pawn).intersects(Bitboard.rank(color.promotablePawnRank))

  protected def pawnsOnBackRank(board: Board, color: Color): Boolean =
    board(color, Pawn).intersects(Bitboard.rank(color.backRank))

  protected def validSide(board: Board, strict: Boolean)(color: Color): Boolean =
    board(color, King).count == 1 &&
      (!strict || { board(color, Pawn).count <= 8 && board(color).count <= 16 }) &&
      !pawnsOnPromotionRank(board, color) &&
      !pawnsOnBackRank(board, color)

  def valid(board: Board, strict: Boolean): Boolean =
    Color.all.forall(validSide(board, strict))

  val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id.value

object Variant:

  given Eq[Variant] = Eq.by(_.id)

  opaque type Id = Int
  object Id extends OpaqueInt[Id]

  opaque type LilaKey = String
  object LilaKey extends OpaqueString[LilaKey]

  opaque type UciKey = String
  object UciKey extends OpaqueString[UciKey]

  object list:
    val all: List[Variant] = List(
      Standard,
      Crazyhouse,
      Chess960,
      FromPosition,
      KingOfTheHill,
      ThreeCheck,
      Antichess,
      Atomic,
      Horde,
      RacingKings
    )
    val byId  = all.mapBy(_.id)
    val byKey = all.mapBy(_.key)

    val openingSensibleVariants: Set[Variant] = Set(
      chess.variant.Standard,
      chess.variant.Crazyhouse,
      chess.variant.ThreeCheck,
      chess.variant.KingOfTheHill
    )
    val divisionSensibleVariants: Set[Variant] = Set(
      chess.variant.Standard,
      chess.variant.Chess960,
      chess.variant.ThreeCheck,
      chess.variant.KingOfTheHill,
      chess.variant.FromPosition
    )

  inline def default: Variant = Standard

  inline def apply(inline id: Id): Option[Variant]       = list.byId.get(id)
  inline def apply(inline key: LilaKey): Option[Variant] = list.byKey.get(key)
  def orDefault(id: Id): Variant                         = apply(id) | default
  def orDefault(key: LilaKey): Variant                   = apply(key) | default
  def idOrDefault(id: Option[Id]): Variant               = id.flatMap(apply(_)) | default
  def orDefault(key: Option[LilaKey]): Variant           = key.flatMap(apply(_)) | default

  def byName(name: String): Option[Variant] =
    list.all.find(_.name.toLowerCase == name.toLowerCase)

  def exists(id: Id): Boolean = list.byId.contains(id)

  private[variant] def symmetricRank(rank: IndexedSeq[Role]): Map[Square, Piece] =
    (File.all.zip(rank).map((x, role) => Square(x, Rank.First) -> (White - role)) ++
      File.all.map(Square(_, Rank.Second) -> White.pawn) ++
      File.all.map(Square(_, Rank.Seventh) -> Black.pawn) ++
      File.all.zip(rank).map((x, role) => Square(x, Rank.Eighth) -> (Black - role))).toMap

  def isValidInitialFen(variant: Variant, fen: Option[Fen.Full], strict: Boolean = false) =
    if variant.chess960
    then fen.forall(f => Chess960.positionNumber(f).isDefined)
    else if variant.fromPosition then
      fen.exists: f =>
        Fen.read(f).exists(_.playable(strict))
    else true
