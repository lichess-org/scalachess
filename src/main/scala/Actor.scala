package chess

import format.Uci
import Pos.posAt

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
) {

  import Actor._

  lazy val moves: List[Move] = kingSafetyMoveFilter(trustedMoves(board.variant.allowsCastling))

  /** The moves without taking defending the king into account */
  def trustedMoves(withCastle: Boolean): List[Move] = {
    val moves = piece.role match {
      case Pawn => pawnDir(pos) map { next =>
        val fwd = Some(next) filterNot board.pieces.contains
        def capture(horizontal: Direction): Option[Move] = {
          for {
            p ← horizontal(next)
            if board.pieces.get(p).exists { _.color != color }
            b ← board.taking(pos, p)
          } yield move(p, b, Some(p))
        } flatMap maybePromote
        def enpassant(horizontal: Direction): Option[Move] = for {
          victimPos ← horizontal(pos)
          if pos.y == color.passablePawnY
          victim ← board(victimPos)
          if victim == !color - Pawn
          targetPos ← horizontal(next)
          victimFrom ← pawnDir(victimPos) flatMap pawnDir
          if history.lastMove.exists {
            case Uci.Move(orig, dest, _) => orig == victimFrom && dest == victimPos
            case _ => false
          }
          b ← board.taking(pos, targetPos, Some(victimPos))
        } yield move(targetPos, b, Some(victimPos), enpassant = true)
        def forward(p: Pos): Option[Move] =
          board.move(pos, p) map { move(p, _) } flatMap maybePromote
        def maybePromote(m: Move): Option[Move] =
          if (m.dest.y == m.color.promotablePawnY)
            (m.after promote m.dest) map { b2 =>
              m.copy(after = b2, promotion = Some(Queen))
            }
          else Some(m)

        List(
          fwd flatMap forward,
          for {
            p ← fwd
            if board.variant.isUnmovedPawn(color, pos)
            p2 ← pawnDir(p)
            if !(board.pieces contains p2)
            b ← board.move(pos, p2)
          } yield move(p2, b),
          capture(_.left),
          capture(_.right),
          enpassant(_.left),
          enpassant(_.right)
        ).flatten
      } getOrElse Nil

      case Bishop => longRange(Bishop.dirs)

      case Knight => shortRange(Knight.dirs)

      case Rook => longRange(Rook.dirs)

      case Queen => longRange(Queen.dirs)

      case King if withCastle => shortRange(King.dirs) ++ castle
      case King => shortRange(King.dirs)
    }

    // We apply the current game variant's effects if there are any so that we can accurately decide if the king would
    // be in danger after the move was made.
    if (board.variant.hasMoveEffects) moves map (_.applyVariantEffect) else moves
  }

  lazy val destinations: List[Pos] = moves map (_.dest)

  def color = piece.color
  def is(c: Color) = c == piece.color
  def is(p: Piece) = p == piece

  /*
   *  Filters out moves that would put the king in check.
   *
   *  critical function. optimize for performance.
   */
  def kingSafetyMoveFilter(ms: List[Move]): List[Move] = {
    val filter: Piece => Boolean =
      if ((piece is King) || check) (_ => true) else (_.role.projection)
    val stableKingPos = if (piece is King) None else board kingPosOf color
    ms filter { m =>
      board.variant.kingSafety(m, filter, stableKingPos orElse (m.after kingPosOf color))
    }
  }

  lazy val check: Boolean = board check color

  private def castle: List[Move] = castleOn(KingSide) ::: castleOn(QueenSide)

  def castleOn(side: Side): List[Move] = (for {
    kingPos ← board kingPosOf color
    if history canCastle color on side
    tripToRook = side.tripToRook(kingPos, board)
    rookPos ← tripToRook.lastOption
    if board(rookPos) contains color.rook
    if history.unmovedRooks.pos.contains(rookPos)
    newKingPos ← posAt(side.castledKingX, kingPos.y)
    travelPoss = kingPos <-> newKingPos
    if !travelPoss.map(board.apply).exists {
      case Some(piece) if piece == color.rook || piece == color.king => false
      case Some(_) => true
      case _ => false
    }
    if !travelPoss.exists(p => board.variant.kingThreatened(board, !color, p))
    newRookPos ← posAt(side.castledRookX, rookPos.y)
    b1 ← board take rookPos
    b2 ← newKingPos match {
      case p if p == kingPos => Some(b1)
      case p => b1.move(kingPos, p)
    }
    b3 ← b2.place(color.rook, newRookPos)
    b4 = b3 updateHistory (_ withoutCastles color)
    castle = Some((kingPos -> newKingPos, rookPos -> newRookPos))
  } yield (board.variant == chess.variant.Chess960).fold(
    List(rookPos),
    List(rookPos, newKingPos).distinct
  ) map { move(_, b4, castle = castle) }) getOrElse Nil

  private def shortRange(dirs: Directions): List[Move] =
    dirs flatMap { _(pos) } flatMap { to =>
      board.pieces.get(to) match {
        case None => board.move(pos, to) map { move(to, _) }
        case Some(piece) =>
          if (piece is color) Nil
          else board.taking(pos, to) map { move(to, _, Some(to)) }
      }
    }

  private def longRange(dirs: Directions): List[Move] = {
    val buf = new ArrayBuffer[Move]

    @tailrec
    def addAll(p: Pos, dir: Direction): Unit = {
      dir(p) match {
        case None => ()
        case s @ Some(to) => board.pieces.get(to) match {
          case None => {
            board.move(pos, to).foreach { buf += move(to, _) }
            addAll(to, dir)
          }
          case Some(piece) =>
            if (piece.color != color) board.taking(pos, to) foreach {
              buf += move(to, _, s)
            }
        }
      }
    }

    dirs foreach { addAll(pos, _) }
    buf.toList
  }

  private val pawnDir = pawnDirOf(color)

  private def move(
    dest: Pos,
    after: Board,
    capture: Option[Pos] = None,
    castle: Option[((Pos, Pos), (Pos, Pos))] = None,
    promotion: Option[PromotableRole] = None,
    enpassant: Boolean = false
  ) = Move(
    piece = piece,
    orig = pos,
    dest = dest,
    situationBefore = Situation(board, piece.color),
    after = after,
    capture = capture,
    castle = castle,
    promotion = promotion,
    enpassant = enpassant
  )

  private def history = board.history
}

object Actor {

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean =
    board.variant.longRangeThreatens(board, p, dir, to)

  def pawnDirOf(color: Color): Direction = color.fold(_.up, _.down)

  /**
   * Determines the position one ahead of a pawn based on the color of the piece.
   * White pawns move up and black pawns move down.
   */
  def posAheadOfPawn(pos: Pos, color: Color): Option[Pos] = pawnDirOf(color)(pos)

  /**
   * Determines the squares that a pawn attacks based on the colour of the pawn.
   */
  def pawnAttacks(pos: Pos, color: Color): List[Pos] = color.fold(
    List(pos.upLeft, pos.upRight),
    List(pos.downLeft, pos.downRight)
  ).flatten
}
