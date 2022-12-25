package chess

import format.Uci
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait IActor:
  lazy val moves: List[Move]
  def trustedMoves(withCastle: Boolean): List[Move]

case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board,
):

  import Actor.*

  lazy val moves: List[Move] = kingSafetyMoveFilter(trustedMoves(board.variant.allowsCastling))

  /** The moves without taking defending the king into account */
  def trustedMoves(withCastle: Boolean): List[Move] =
    val moves = piece.role match
      case Pawn =>
        pawnDir(pos) map { next =>
          val fwd = Option(next) filterNot board.contains
          def capture(horizontal: Direction): Option[Move] = {
            for {
              p <- horizontal(next)
              if board(p).exists { _.color != color }
              b <- board.taking(pos, p)
            } yield move(p, b, Option(p))
          } flatMap maybePromote
          def enpassant(horizontal: Direction): Option[Move] =
            for {
              victimPos <- horizontal(pos).filter(_ => pos.rank == color.passablePawnRank)
              _         <- board(victimPos).filter(v => v == !color - Pawn)
              targetPos <- horizontal(next)
              _ <- pawnDir(victimPos) flatMap pawnDir filter { vf =>
                history.lastMove.exists {
                  case Uci.Move(orig, dest, _) => orig == vf && dest == victimPos
                  case _                       => false
                }
              }
              b <- board.taking(pos, targetPos, Option(victimPos))
            } yield move(targetPos, b, Option(victimPos), enpassant = true)
          def forward(p: Pos): Option[Move] =
            board.move(pos, p) map { move(p, _) } flatMap maybePromote
          def maybePromote(m: Move): Option[Move] =
            if (m.dest.rank == m.color.promotablePawnRank)
              (m.after promote m.dest) map { b2 =>
                m.copy(after = b2, promotion = Option(Queen))
              }
            else Option(m)

          List(
            fwd flatMap forward,
            for {
              p  <- fwd.filter(_ => board.variant.isUnmovedPawn(color, pos))
              p2 <- pawnDir(p)
              if !(board.contains(p2))
              b <- board.move(pos, p2)
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

      case King if withCastle => shortRange(King.dirs) ::: castle
      case King               => shortRange(King.dirs)

    // We apply the current game variant's effects if there are any so that we can accurately decide if the king would
    // be in danger after the move was made.
    if (board.variant.hasMoveEffects) moves map (_.applyVariantEffect) else moves

  lazy val destinations: List[Pos] = moves.map(_.dest)

  inline def color               = piece.color
  inline def is(inline c: Color) = c == piece.color
  inline def is(inline r: Role)  = r == piece.role
  inline def is(inline p: Piece) = p == piece

  /*
   *  Filters out moves that would put the king in check.
   *
   *  critical function. optimize for performance.
   */
  def kingSafetyMoveFilter(ms: List[Move]): List[Move] =
    val filter: Piece => Boolean =
      if ((piece is King) || check) _ => true else _.role.projection
    val stableKingPos = if (piece is King) None else board kingPosOf color
    ms filter { m =>
      board.variant.kingSafety(m, filter, stableKingPos orElse (m.after kingPosOf color))
    }

  lazy val check: Boolean = board check color

  private def castle: List[Move] = castleOn(KingSide) ::: castleOn(QueenSide)

  def castleOn(side: Side): List[Move] =
    (for {
      // Check castling rights.
      kingPos <- board kingPosOf color filter (_ => history canCastle color on side)
      rookPos <- side.tripToRook(kingPos, board).lastOption
      if board(rookPos) contains color.rook
      if history.unmovedRooks.value.contains(rookPos)
      // Check impeded castling.
      newKingPos       = Pos(side.castledKingFile, kingPos.rank)
      newRookPos       = Pos(side.castledRookFile, rookPos.rank)
      kingPath         = kingPos <-> newKingPos
      rookPath         = rookPos <-> newRookPos
      mustBeUnoccupied = (kingPath ++ rookPath).filter(_ != kingPos).filter(_ != rookPos)
      if !mustBeUnoccupied.exists(board.contains)
      // Check the king is not currently attacked, and none of the squares it
      // passes *through* are attacked. We do this after removing the old king,
      // to ensure the old king does not shield attacks. This is important in
      // Atomic chess, where touching kings can shield attacks without being in
      // check.
      b1 <- board take kingPos
      mustNotBeAttacked = kingPath.filter(_ != newKingPos || kingPos == newKingPos)
      if !mustNotBeAttacked.exists(p => board.variant.kingThreatened(b1, !color, p))
      // Test the final king position seperately, after the rook has been moved.
      b2 <- b1 take rookPos
      b3 <- b2.place(color.king, newKingPos)
      b4 <- b3.place(color.rook, newRookPos)
      if !board.variant.kingThreatened(b4, !color, newKingPos)
      b5     = b4 updateHistory (_ withoutCastles color)
      castle = Option((kingPos -> newKingPos, rookPos -> newRookPos))
    } yield {
      rookPos :: {
        if (kingPos.file == File.E && List(File.A, File.H).contains(rookPos.file) && !board.variant.chess960)
          newKingPos :: Nil
        else Nil
      }
    } map { move(_, b5, castle = castle) }) getOrElse Nil

  private def shortRange(dirs: Directions): List[Move] =
    dirs flatMap { _(pos) } flatMap { to =>
      board(to) match
        case None => board.move(pos, to) map { move(to, _) }
        case Some(piece) =>
          if (piece is color) Nil
          else board.taking(pos, to) map { move(to, _, Option(to)) }
    }

  private def longRange(dirs: Directions): List[Move] =
    val buf = new ArrayBuffer[Move]

    @tailrec
    def addAll(p: Pos, dir: Direction): Unit =
      dir(p) match
        case None => ()
        case s @ Some(to) =>
          board(to) match
            case None =>
              board.move(pos, to).foreach { buf += move(to, _) }
              addAll(to, dir)
            case Some(piece) =>
              if (piece.color != color) board.taking(pos, to) foreach {
                buf += move(to, _, s)
              }

    dirs foreach { addAll(pos, _) }
    buf.toList

  private inline def pawnDir = pawnDirOf(color)

  private def move(
      dest: Pos,
      after: Board,
      capture: Option[Pos] = None,
      castle: Option[((Pos, Pos), (Pos, Pos))] = None,
      promotion: Option[PromotableRole] = None,
      enpassant: Boolean = false
  ) =
    Move(
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

object Actor:

  inline def pawnDirOf(inline color: Color): Direction = color.fold(_.up, _.down)

  /** Determines the position one ahead of a pawn based on the color of the piece.
    * White pawns move up and black pawns move down.
    */
  def posAheadOfPawn(pos: Pos, color: Color): Option[Pos] = pawnDirOf(color)(pos)

  /** Determines the squares that a pawn attacks based on the colour of the pawn.
    */
  def pawnAttacks(pos: Pos, color: Color): List[Pos] =
    color
      .fold(
        List(pos.upLeft, pos.upRight),
        List(pos.downLeft, pos.downRight)
      )
      .flatten
