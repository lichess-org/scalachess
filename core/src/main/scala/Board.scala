package chess

import cats.syntax.all.*
import chess.Bitboard.*

// Chess board representation
case class Board(occupied: Bitboard, byColor: ByColor[Bitboard], byRole: ByRole[Bitboard]):

  export byColor.{ white, black }
  export byRole.{
    pawn as pawns,
    knight as knights,
    bishop as bishops,
    rook as rooks,
    queen as queens,
    king as kings
  }

  def nbPieces: Int = occupied.count

  /* whether a square occupied by a piece */
  def isOccupied(s: Square): Boolean = occupied.contains(s)

  /* whether a piece exists */
  def contains(p: Piece): Boolean =
    piece(p).nonEmpty

  /* whether a piece exists */
  def contains(color: Color, role: Role): Boolean =
    piece(color, role).nonEmpty

  /* bitboard of a given piece */
  def byPiece(piece: Piece): Bitboard =
    byPiece(piece.color, piece.role)

  /* bitboard of a piece by given color and role */
  def byPiece(color: Color, role: Role): Bitboard =
    byColor(color) & byRole(role)

  /* bitboard of pieces by given color and roles */
  def byPiece(color: Color, roles: Role*): Bitboard =
    byColor(color) & byRole(roles*)

  /* return a Role at a given square if any */
  def roleAt(s: Square): Option[Role] =
    byRole.findRole(_.contains(s))

  /* return a Role at a square by given file and rank if any */
  def roleAt(file: File, rank: Rank): Option[Role] =
    byRole.findRole(_.contains(file, rank))

  /* return a Color at a square by if any */
  def colorAt(s: Square): Option[Color] =
    byColor.findColor(_.contains(s))

  /* return a Color at a square by given file and rank if any */
  def colorAt(file: File, rank: Rank): Option[Color] =
    byColor.findColor(_.contains(file, rank))

  def pieceAt(file: File, rank: Rank): Option[Piece] =
    (colorAt(file, rank), roleAt(file, rank)).mapN(Piece.apply)

  def pieceAt(s: Square): Option[Piece] =
    pieceAt(s.file, s.rank)

  def piece(p: Piece): Bitboard =
    piece(p.color, p.role)

  def piece(color: Color, role: Role): Bitboard =
    byColor(color) & byRole(role)

  def whiteAt(s: Square): Boolean =
    white.contains(s)

  def blackAt(s: Square): Boolean =
    black.contains(s)

  def byRole(roles: Role*): Bitboard =
    roles.foldLeft(Bitboard.empty)((acc, r) => acc | byRole(r))

  def byRoleOf(color: Color): ByRole[Bitboard] =
    byRole.map(_ & byColor(color))

  def kings(color: Color): List[Square] =
    kingOf(color).squares

  def kingOf(c: Color): Bitboard =
    kings & byColor(c)

  def kingPosOf(c: Color): Option[Square] =
    kingOf(c).singleSquare

  def kingsAndBishopsOnly: Boolean =
    (kings | bishops) == occupied

  def kingsAndKnightsOnly: Boolean =
    (kings | knights) == occupied

  def onlyKnights: Boolean =
    knights == occupied

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

  def kingsOnly: Boolean =
    kings == occupied

  def kingsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings)

  def kingsAndKnightsOnlyOf(color: Color): Boolean =
    onlyOf(color, kings | knights)

  def onlyOf(color: Color, mask: Bitboard): Boolean =
    byColor(color).subsetOf(mask)

  /* Tests whether a color has only pieces of given roles
   * e.g. onlyOf(Color.White, Role.King, Role.Queen) means that White has only King and Queen
   * and no other pieces
   */
  def onlyOf(color: Color, roles: Role*): Boolean =
    byColor(color).subsetOf(byRole(roles*))

  def nonKingsOf(color: Color): Bitboard =
    byColor(color) & ~kings

  def nonKing: Bitboard =
    occupied & ~kings

  /* Count number of a given piece */
  def count(p: Piece): Int =
    piece(p).count

  /* Count number of a piece by given color and role */
  def count(color: Color, role: Role): Int =
    (byColor(color) & byRole(role)).count

  /* Count number of pieces by a given color */
  def count(c: Color): Int =
    byColor(c).count

  def sliders: Bitboard =
    bishops ^ rooks ^ queens

  def attacks(s: Square, attacker: Color): Boolean =
    attackers(s, attacker).nonEmpty

  def attackers(s: Square, attacker: Color): Bitboard =
    attackers(s, attacker, occupied)

  def attackers(s: Square, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.kingAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
    )

  def squaresAttackedByPawns(attacker: Color): Bitboard =
    var enemyPawnAttacks = Bitboard.empty
    byPiece(attacker, Pawn).foreach { sq =>
      enemyPawnAttacks |= sq.pawnAttacks(attacker)
    }
    enemyPawnAttacks

  /* is a king of this color in check */
  def isCheck(color: Color): Check =
    Check(kings(color).exists(attacks(_, !color)))

  def checkers(color: Color): Bitboard =
    kingPosOf(color).fold(Bitboard.empty)(attackers(_, !color))

  /** Find all blockers between the king and attacking sliders First we find all snipers (all potential sliders which
    * can attack the king) Then we loop over those snipers if there is only one blockers between the king and the sniper
    * we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(ourKing: Square, us: Color): Bitboard =
    val snipers = byColor(!us) & (
      ourKing.rookAttacks(Bitboard.empty) & (rooks ^ queens) |
        ourKing.bishopAttacks(Bitboard.empty) & (bishops ^ queens)
    )
    snipers.fold(Bitboard.empty): (blockers, sniper) =>
      val between = Bitboard.between(ourKing, sniper) & occupied
      if between.moreThanOne then blockers
      else blockers | between

  /* ====== Board manipulation ====== */

  def discard(s: Square): Board =
    discard(s.bb)

  def discard(mask: Bitboard): Board =
    val notMask = ~mask
    Board(
      occupied & notMask,
      byColor.map(_ & notMask),
      byRole.map(_ & notMask)
    )

  // put a piece to an empty square
  def put(piece: Piece, at: Square): Option[Board] =
    Option.unless(isOccupied(at))(putOrReplace(piece, at))

  // put a piece to an occupied square
  def replace(piece: Piece, at: Square): Option[Board] =
    Option.when(isOccupied(at))(putOrReplace(piece, at))

  // put a piece into the board
  def putOrReplace(s: Square, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bl
    Board(
      b.occupied | m,
      b.byColor.update(color, _ | m),
      b.byRole.update(role, _ | m)
    )

  // put a piece into the board
  // remove the existing piece at that square if needed
  def putOrReplace(p: Piece, s: Square): Board =
    putOrReplace(s, p.role, p.color)

  def take(at: Square): Option[Board] =
    isOccupied(at).option(discard(at))

  // move without capture
  def move(orig: Square, dest: Square): Option[Board] =
    if isOccupied(dest) then None
    else pieceAt(orig).map(discard(orig).putOrReplace(_, dest))

  def taking(orig: Square, dest: Square, taking: Option[Square] = None): Option[Board] =
    for
      piece <- pieceAt(orig)
      takenSquare = taking.getOrElse(dest)
      if isOccupied(takenSquare)
    yield discard(orig).discard(takenSquare).putOrReplace(piece, dest)

  def promote(orig: Square, dest: Square, piece: Piece): Option[Board] =
    take(orig).map(_.putOrReplace(piece, dest))

  // benchmarked: https://github.com/lichess-org/scalachess/pull/438
  lazy val pieceMap: Map[Square, Piece] =
    val m = Map.newBuilder[Square, Piece]
    byColor.foreach: (color, c) =>
      byRole.foreach: (role, r) =>
        val piece = color - role
        (c & r).foreach: s =>
          m += s -> piece
    m.result

  def piecesOf(c: Color): Map[Square, Piece] =
    pieceMap.filter((_, p) => p.color == c)

  def pieces: List[Piece] = pieces(occupied)

  def pieces(occupied: Bitboard): List[Piece] =
    occupied.flatMap(pieceAt)

  def fold[B](init: B)(f: (B, Color, Role) => B): B =
    var m = init
    byColor.foreach: (color, c) =>
      byRole.foreach: (role, r) =>
        (c & r).foreach: _ =>
          m = f(m, color, role)
    m

  def fold[B](init: B)(f: (B, Color, Role, Square) => B): B =
    var m = init
    byColor.foreach: (color, c) =>
      byRole.foreach: (role, r) =>
        (c & r).foreach: s =>
          m = f(m, color, role, s)
    m

  def foreach[U](f: (Color, Role, Square) => U): Unit =
    byColor.foreach: (color, c) =>
      byRole.foreach: (role, r) =>
        (c & r).foreach: s =>
          f(color, role, s)

object Board:

  val empty: Board = Board(
    Bitboard.empty,
    ByColor.fill(Bitboard.empty),
    ByRole.fill(Bitboard.empty)
  )

  val standard = Board(
    occupied = Bitboard(0xffff00000000ffffL),
    byColor = ByColor(
      white = Bitboard(0x000000000000ffffL),
      black = Bitboard(0xffff000000000000L)
    ),
    byRole = ByRole(
      pawn = Bitboard(0x00ff00000000ff00L),
      knight = Bitboard(0x4200000000000042L),
      bishop = Bitboard(0x2400000000000024L),
      rook = Bitboard(0x8100000000000081L),
      queen = Bitboard(0x0800000000000008L),
      king = Bitboard(0x1000000000000010L)
    )
  )
  def fromMap(pieces: PieceMap): Board =
    var pawns = Bitboard.empty
    var knights = Bitboard.empty
    var bishops = Bitboard.empty
    var rooks = Bitboard.empty
    var queens = Bitboard.empty
    var kings = Bitboard.empty
    var white = Bitboard.empty
    var black = Bitboard.empty
    var occupied = Bitboard.empty

    pieces.foreach: (s, p) =>
      val position = s.bb
      occupied |= position
      p.role match
        case Pawn => pawns |= position
        case Knight => knights |= position
        case Bishop => bishops |= position
        case Rook => rooks |= position
        case Queen => queens |= position
        case King => kings |= position

      p.color match
        case Color.White => white |= position
        case Color.Black => black |= position

    Board(occupied, ByColor(white, black), ByRole(pawns, knights, bishops, rooks, queens, kings))
