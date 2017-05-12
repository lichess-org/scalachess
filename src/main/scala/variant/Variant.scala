package chess
package variant

import scala.collection.breakOut
import scalaz.Validation.FlatMap._

import Pos.posAt

abstract class Variant(
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean
) {

  def pieces: Map[Pos, Piece]

  def standard = this == Standard
  def chess960 = this == Chess960
  def fromPosition = this == FromPosition
  def kingOfTheHill = this == KingOfTheHill
  def threeCheck = this == ThreeCheck
  def antichess = this == Antichess
  def atomic = this == Atomic
  def horde = this == Horde
  def racingKings = this == RacingKings
  def crazyhouse = this == Crazyhouse

  def exotic = !standard

  def allowsCastling = !castles.isEmpty

  protected val backRank = Vector(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

  def castles: Castles = Castles.all

  def initialFen = format.Forsyth.initial

  def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop) => true
    case _ => false
  }

  def validMoves(situation: Situation): Map[Pos, List[Move]] = situation.actors.collect {
    case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
  }(breakOut)

  // Optimised for performance
  def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true): Boolean = {
    board.pieces exists {
      case (pos, piece) if piece.color == color && filter(piece) && piece.eyes(pos, to) =>
        (!piece.role.projection) || piece.role.dir(pos, to).exists {
          longRangeThreatens(board, pos, _, to)
        }
      case _ => false
    }
  }

  def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean = !{
    kingPos exists { kingThreatened(m.after, !m.color, _, filter) }
  }

  def kingSafety(a: Actor, m: Move): Boolean = kingSafety(
    m,
    if (a.piece is King) (_ => true) else if (a.check) (_.role.attacker) else (_.role.projection),
    if (a.piece.role == King) None else a.board kingPosOf a.color
  )

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean = dir(p) exists { next =>
    next == to || (!board.pieces.contains(next) && longRangeThreatens(board, next, dir, to))
  }

  def move(situation: Situation, from: Pos, to: Pos, promotion: Option[PromotableRole]): Valid[Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) = situation.moves get from flatMap (_.find(_.dest == to))

    for {
      actor ← situation.board.actors get from toValid "No piece on " + from
      myActor ← actor.validIf(actor is situation.color, "Not my piece on " + from)
      m1 ← findMove(from, to) toValid "Piece on " + from + " cannot move to " + to
      m2 ← m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3 <- m2 validIf (isValidPromotion(promotion), "Cannot promote to " + promotion + " in this game mode")
    } yield m3
  }

  def drop(situation: Situation, role: Role, pos: Pos): Valid[Drop] =
    s"$this variant cannot drop pieces".failureNel

  def staleMate(situation: Situation): Boolean = !situation.check && situation.moves.isEmpty

  def checkmate(situation: Situation) = situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Some(!situation.color) else None

  def specialEnd(situation: Situation) = false

  def specialDraw(situation: Situation) = false

  /**
   * Returns true if neither player can win
   */
  def insufficientWinningMaterial(board: Board) = InsufficientMatingMaterial(board)

  /**
   * Returns true if the player of the given colour has insufficient material to win.
   * This can be used to determine whether a player losing on time against a player
   * who doesn't have enough material to win should draw instead.
   */
  def insufficientWinningMaterial(board: Board, color: Color) = InsufficientMatingMaterial(board, color)

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /**
   * Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
   * example
   */
  def addVariantEffect(move: Move): Move = move

  def updatePositionHashes(board: Board, move: Move, hash: chess.PositionHash): PositionHash =
    Hash(Situation(board, !move.piece.color)) ++ {
      if ((move.piece is Pawn) || move.captures || move.promotes) Array(): PositionHash
      else hash
    }

  def updatePositionHashes(board: Board, drop: Drop, hash: chess.PositionHash): PositionHash = Array()

  /**
   * Once a move has been decided upon from the available legal moves, the board is finalized
   */
  def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board = board

  protected def pawnsOnPromotionRank(board: Board, color: Color) = {
    board.pieces.exists {
      case (pos, Piece(c, r)) if c == color && r == Pawn && pos.y == color.promotablePawnY => true
      case _ => false
    }
  }

  protected def validSide(board: Board, strict: Boolean)(color: Color) = {
    val roles = board rolesOf color
    roles.count(_ == King) == 1 &&
      (!strict || { roles.count(_ == Pawn) <= 8 && roles.size <= 16 }) &&
      !pawnsOnPromotionRank(board, color)
  }

  def valid(board: Board, strict: Boolean) = Color.all forall validSide(board, strict)_

  val roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn)

  val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  lazy val rolesByPgn: Map[Char, Role] = roles.map { r => (r.pgn, r) }(breakOut)

  lazy val rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles.map { r => (r.pgn, r) }(breakOut)

  def isUnmovedPawn(color: Color, pos: Pos) = pos.y == color.fold(2, 7)

  override def toString = s"Variant($name)"
}

object Variant {

  val all = List(Standard, Crazyhouse, Chess960, FromPosition, KingOfTheHill, ThreeCheck, Antichess, Atomic, Horde, RacingKings, Extinction)
  val byId = all map { v => (v.id, v) } toMap
  val byKey = all map { v => (v.key, v) } toMap

  val default = Standard

  def apply(id: Int): Option[Variant] = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant = apply(id) | default
  def orDefault(key: String): Variant = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

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
    chess.variant.Antichess,
    chess.variant.FromPosition
  )

  private[variant] def symmetricRank(rank: IndexedSeq[Role]): Map[Pos, Piece] =
    (for (y ← Seq(1, 2, 7, 8); x ← 1 to 8) yield {
      posAt(x, y) map { pos =>
        (pos, y match {
          case 1 => White - rank(x - 1)
          case 2 => White.pawn
          case 7 => Black.pawn
          case 8 => Black - rank(x - 1)
        })
      }
    }).flatten.toMap
}
