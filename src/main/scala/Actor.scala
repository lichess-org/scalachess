package chess

import Pos.posAt

case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board) {

  import Actor._

  lazy val moves: List[Move] = kingSafety(piece.role match {

    case Bishop ⇒ longRange(Bishop.dirs)

    case Queen  ⇒ longRange(Queen.dirs)

    case Knight ⇒ shortRange(Knight.dirs)

    case King   ⇒ shortRange(King.dirs) ++ castle

    case Rook   ⇒ longRange(Rook.dirs)

    case Pawn ⇒ pawnDir(pos) map { next ⇒
      val fwd = Some(next) filterNot board.occupations
      def capture(horizontal: Direction): Option[Move] = {
        for {
          p ← horizontal(next)
          if enemies(p)
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
        if history.lastMove == Some(victimFrom, victimPos)
        b ← board.taking(pos, targetPos, Some(victimPos))
      } yield move(targetPos, b, Some(victimPos), enpassant = true)
      def forward(p: Pos): Option[Move] =
        board.move(pos, p) map { move(p, _) } flatMap maybePromote
      def maybePromote(m: Move): Option[Move] =
        if (m.dest.y == m.color.promotablePawnY)
          (m.after promote m.dest) map { b2 ⇒
            m.copy(after = b2, promotion = Some(Queen))
          }
        else Some(m)

      List(
        fwd flatMap forward,
        for {
          p ← fwd; if pos.y == color.unmovedPawnY
          p2 ← pawnDir(p); if !(board occupations p2)
          b ← board.move(pos, p2)
        } yield move(p2, b),
        capture(_.left),
        capture(_.right),
        enpassant(_.left),
        enpassant(_.right)
      ).flatten
    } getOrElse Nil
  })

  lazy val destinations: List[Pos] = moves map (_.dest)

  def color = piece.color
  def is(c: Color) = c == piece.color
  def is(p: Piece) = p == piece

  def hash: String = piece.forsyth + pos.key

  // critical function. optimize for performance
  private def kingSafety(ms: List[Move]): List[Move] = {
    val filter: Piece ⇒ Boolean =
      if (piece is King) (_ ⇒ true) else if (check) (_.role.attacker) else (_.role.projection)
    val stableKingPos = if (piece.role == King) None else board kingPosOf color
    ms filterNot { m ⇒
      stableKingPos orElse (m.after kingPosOf color) exists { kingPos ⇒
        threatens(m.after, !color, kingPos, filter)
      }
    }
  }

  def attacker = piece.role.attacker
  def projection = piece.role.projection

  lazy val check: Boolean = board check color

  private def castle: List[Move] = castleOn(KingSide) ::: castleOn(QueenSide)

  def castleOn(side: Side): List[Move] = (for {
    kingPos ← board kingPosOf color
    if history canCastle color on side
    tripToRook = side.tripToRook(kingPos, board)
    rookPos ← tripToRook.lastOption
    if board(rookPos) == Some(color.rook)
    newKingPos ← posAt(side.castledKingX, kingPos.y)
    securedPoss = kingPos <-> newKingPos
    if !securedPoss.exists(p ⇒ threatens(board, !color, p))
    newRookPos ← posAt(side.castledRookX, rookPos.y)
    b1 ← board take rookPos
    b2 ← newKingPos match {
      case p if p == kingPos ⇒ Some(b1)
      case p                 ⇒ b1.move(kingPos, p)
    }
    b3 ← b2.place(color.rook, newRookPos)
    b4 = b3 updateHistory (_ withoutCastles color)
    castle = Some((kingPos -> newKingPos, rookPos -> newRookPos))
  } yield (board.variant == Variant.Chess960).fold(
    List(rookPos),
    List(rookPos, newKingPos).distinct
  ) map { move(_, b4, castle = castle) }) getOrElse Nil

  private def shortRange(dirs: Directions): List[Move] =
    (pos mapply dirs).flatten filterNot friends map { to ⇒
      if (enemies(to)) board.taking(pos, to) map { move(to, _, Some(to)) }
      else board.move(pos, to) map { move(to, _) }
    } flatten

  private def longRange(dirs: Directions): List[Move] = {

    def forward(p: Pos, dir: Direction): List[Move] = dir(p) match {
      case None                        ⇒ Nil
      case Some(next) if friends(next) ⇒ Nil
      case Some(next) if enemies(next) ⇒ board.taking(pos, next) map { b ⇒
        move(next, b, Some(next))
      } toList
      case Some(next) ⇒ board.move(pos, next) map { b ⇒
        move(next, b) :: forward(next, dir)
      } getOrElse Nil
    }

    dirs flatMap { dir ⇒ forward(pos, dir) }
  }

  private def longRangePoss(dirs: Directions): List[Pos] = {

    def forward(p: Pos, dir: Direction): List[Pos] = dir(p) match {
      case None                        ⇒ Nil
      case Some(next) if friends(next) ⇒ Nil
      case Some(next) if enemies(next) ⇒ List(next)
      case Some(next)                  ⇒ next :: forward(next, dir)
    }

    dirs flatMap { dir ⇒ forward(pos, dir) }
  }

  private val pawnDir = pawnDirOf(color)

  private def move(
    dest: Pos,
    after: Board,
    capture: Option[Pos] = None,
    castle: Option[((Pos, Pos), (Pos, Pos))] = None,
    promotion: Option[PromotableRole] = None,
    enpassant: Boolean = false) = Move(
    piece = piece,
    orig = pos,
    dest = dest,
    before = board,
    after = after,
    capture = capture,
    castle = castle,
    promotion = promotion,
    enpassant = enpassant)

  private def history = board.history
  private def friends = board occupation color
  private def enemies = board occupation !color
}

object Actor {

  // critical function. optimize for performance
  def threatens(board: Board, color: Color, to: Pos, filter: Piece ⇒ Boolean = _ ⇒ true): Boolean =
    board.pieces exists {
      case (pos, piece) if piece.color == color && filter(piece) ⇒ piece.role match {
        case x: Projection ⇒ x.dir(pos, to) exists {
          longRangeThreatens(board, pos, _, to)
        }
        case _ ⇒ piece.eyes(pos, to)
      }
      case _ ⇒ false
    }

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean = dir(p) exists { next ⇒
    next == to || (!board.pieces.contains(next) && longRangeThreatens(board, next, dir, to))
  }

  private def pawnDirOf(color: Color): Direction = if (color.white) _.up else _.down
}
