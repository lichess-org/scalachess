package chess
package bitboard

import cats.syntax.all.*

import Bitboard.*

// Chess board representation
case class Board(
    occupied: Bitboard,
    byColor: ByColor,
    byRole: ByRole
):
  val white   = byColor.white
  val black   = byColor.black
  val pawns   = byRole.pawn
  val knights = byRole.knight
  val bishops = byRole.bishop
  val rooks   = byRole.rook
  val queens  = byRole.queen
  val kings   = byRole.king

  def sliders                        = bishops ^ rooks ^ queens
  def isOccupied(s: Square): Boolean = occupied.contains(s)

  lazy val nbPieces = occupied.count

  def byPiece(piece: Piece): Bitboard =
    byColor(piece.color) & byRole(piece.role)

  def roleAt(s: Square): Option[Role] =
    byRole.findRole(_.contains(s))

  def colorAt(s: Square): Option[Color] =
    byColor.findColor(_.contains(s))

  def pieceAt(s: Square): Option[Piece] =
    for
      color <- colorAt(s)
      role  <- roleAt(s)
    yield Piece(color, role)

  def whiteAt(s: Square): Boolean =
    white.contains(s)

  def blackAt(s: Square): Boolean =
    black.contains(s)

  def kings(color: Color): List[Square] =
    kingOf(color).squares

  def kingOf(c: Color): Bitboard          = kings & byColor(c)
  def kingPosOf(c: Color): Option[Square] = kingOf(c).singleSquare

  def attackers(s: Square, attacker: Color): Bitboard =
    attackers(s, attacker, occupied)

  def attacks(s: Square, attacker: Color): Boolean =
    attackers(s, attacker).nonEmpty

  def attackers(s: Square, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.kingAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
    )

  // is a king of this color in check
  def isCheck(color: Color): Check =
    Check(kings(color).exists(attacks(_, !color)))

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

  def discard(s: Square): Board =
    discard(s.bb)

  def discard(mask: Bitboard): Board =
    val notMask = ~mask
    Board(
      occupied & notMask,
      byColor.map(_ & notMask),
      byRole.map(_ & notMask)
    )

  def byRoleOf(color: Color): chess.ByRole[Bitboard] = byRole.mapTo(_ & byColor(color))

  // put a piece to an empty square
  def put(piece: Piece, at: Square): Option[Board] =
    (!isOccupied(at)).option(putOrReplace(piece, at))

  // put a piece to an occupied square
  def replace(piece: Piece, at: Square): Option[Board] =
    isOccupied(at).option(putOrReplace(piece, at))

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

  inline def isOccupied(inline p: Piece) =
    piece(p).nonEmpty

  // benchmarked: https://github.com/lichess-org/scalachess/pull/438
  lazy val pieceMap: Map[Square, Piece] =
    val m = Map.newBuilder[Square, Piece]
    byColor.foreach: (color, c) =>
      byRole.foreach: (role, r) =>
        val piece = color - role
        (c & r).foreach: s =>
          m += s -> piece
    m.result

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

  def piecesOf(c: Color): Map[Square, Piece] =
    pieceMap.filter((_, p) => p.color == c)

  def pieces: List[Piece] = pieces(occupied)

  def pieces(occupied: Bitboard): List[Piece] =
    occupied.flatMap(pieceAt)

  def color(c: Color): Bitboard = byColor(c)

  def piece(p: Piece): Bitboard = byColor(p.color) & byRole(p.role)

object Board:

  val empty: Board = Board(
    Bitboard.empty,
    ByColor.fill(Bitboard.empty),
    ByRole.fill(Bitboard.empty)
  )

  def apply(
      occupied: Bitboard,
      white: Bitboard,
      black: Bitboard,
      pawns: Bitboard,
      knights: Bitboard,
      bishops: Bitboard,
      rooks: Bitboard,
      queens: Bitboard,
      kings: Bitboard
  ): Board =
    Board(occupied, ByColor(white, black), ByRole(pawns, knights, bishops, rooks, queens, kings))

  def fromMap(pieces: PieceMap): Board =
    var pawns    = Bitboard.empty
    var knights  = Bitboard.empty
    var bishops  = Bitboard.empty
    var rooks    = Bitboard.empty
    var queens   = Bitboard.empty
    var kings    = Bitboard.empty
    var white    = Bitboard.empty
    var black    = Bitboard.empty
    var occupied = Bitboard.empty

    pieces.foreach: (s, p) =>
      val position = s.bb
      occupied |= position
      p.role match
        case Pawn   => pawns |= position
        case Knight => knights |= position
        case Bishop => bishops |= position
        case Rook   => rooks |= position
        case Queen  => queens |= position
        case King   => kings |= position

      p.color match
        case Color.White => white |= position
        case Color.Black => black |= position

    Board(occupied, ByColor(white, black), ByRole(pawns, knights, bishops, rooks, queens, kings))

private case class ByColor(white: Bitboard, black: Bitboard):

  inline def apply(inline color: Color) = if color.white then white else black

  inline def findColor(pred: Bitboard => Boolean): Option[Color] =
    if pred(white) then White.some
    else if pred(black) then Black.some
    else None

  def foreach(f: (Color, Bitboard) => Unit): Unit =
    f(White, white)
    f(Black, black)

  inline def update(inline color: Color, f: Bitboard => Bitboard): ByColor =
    if color.white then copy(white = f(white))
    else copy(black = f(black))

  def mapReduce[B, C](f: Bitboard => B)(r: (B, B) => C): C = r(f(white), f(black))

  def map(f: Bitboard => Bitboard): ByColor = ByColor(f(white), f(black))

private object ByColor:
  inline def fill(inline a: Bitboard): ByColor = ByColor(a, a)

private case class ByRole(
    pawn: Bitboard,
    knight: Bitboard,
    bishop: Bitboard,
    rook: Bitboard,
    queen: Bitboard,
    king: Bitboard
):
  def apply(role: Role): Bitboard = role match
    case Pawn   => pawn
    case Knight => knight
    case Bishop => bishop
    case Rook   => rook
    case Queen  => queen
    case King   => king

  inline def update(role: Role, f: Bitboard => Bitboard): ByRole = role match
    case Pawn   => copy(pawn = f(pawn))
    case Knight => copy(knight = f(knight))
    case Bishop => copy(bishop = f(bishop))
    case Rook   => copy(rook = f(rook))
    case Queen  => copy(queen = f(queen))
    case King   => copy(king = f(king))

  inline def find(f: Bitboard => Boolean): Option[Bitboard] =
    if f(pawn) then Some(pawn)
    else if f(knight) then Some(knight)
    else if f(bishop) then Some(bishop)
    else if f(rook) then Some(rook)
    else if f(queen) then Some(queen)
    else if f(king) then Some(king)
    else None

  inline def foreach(f: (Role, Bitboard) => Unit): Unit =
    f(Pawn, pawn)
    f(Knight, knight)
    f(Bishop, bishop)
    f(Rook, rook)
    f(Queen, queen)
    f(King, king)

  inline def findRole(f: Bitboard => Boolean): Option[Role] =
    if f(pawn) then Some(Pawn)
    else if f(knight) then Some(Knight)
    else if f(bishop) then Some(Bishop)
    else if f(rook) then Some(Rook)
    else if f(queen) then Some(Queen)
    else if f(king) then Some(King)
    else None

  inline def map(f: Bitboard => Bitboard): ByRole =
    ByRole(
      f(pawn),
      f(knight),
      f(bishop),
      f(rook),
      f(queen),
      f(king)
    )

  def mapTo[B](f: Bitboard => B): chess.ByRole[B] =
    chess.ByRole(f(pawn), f(knight), f(bishop), f(rook), f(queen), f(king))

private object ByRole:

  inline def fill(inline a: Bitboard): ByRole = ByRole(a, a, a, a, a, a)
