package chess
package bitboard

import Bitboard.*

// Chess board representation
case class Board(
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
    black: Bitboard,
    occupied: Bitboard
):
  def isOccupied(s: Pos): Boolean = occupied.contains(s)

  def sliders = bishops ^ rooks ^ queens

  lazy val byColor = Color.Map(white, black)

  lazy val nbPieces = occupied.count

  def byRole(role: Role): Bitboard =
    role match
      case Pawn   => pawns
      case Knight => knights
      case Bishop => bishops
      case Rook   => rooks
      case Queen  => queens
      case King   => kings

  def roleAt(s: Pos): Option[Role] =
    if pawns.contains(s) then Some(Pawn)
    else if knights.contains(s) then Some(Knight)
    else if bishops.contains(s) then Some(Bishop)
    else if rooks.contains(s) then Some(Rook)
    else if queens.contains(s) then Some(Queen)
    else if kings.contains(s) then Some(King)
    else None

  def colorAt(s: Pos): Option[Color] =
    if white.contains(s) then Some(Color.White)
    else if black.contains(s) then Some(Color.Black)
    else None

  def pieceAt(s: Pos): Option[Piece] =
    for
      color <- colorAt(s)
      role  <- roleAt(s)
    yield Piece(color, role)

  def whiteAt(s: Pos): Boolean =
    white.contains(s)

  def blackAt(s: Pos): Boolean =
    black.contains(s)

  def kings(color: Color): List[Pos] =
    (kings & byColor(color)).occupiedSquares

  def kingOf(c: Color): Bitboard       = kings & byColor(c)
  def kingPosOf(c: Color): Option[Pos] = kingOf(c).singleSquare

  def attackers(s: Pos, attacker: Color): Bitboard =
    attackers(s, attacker, occupied)

  def attacks(s: Pos, attacker: Color): Boolean =
    attackers(s, attacker).nonEmpty

  def attackers(s: Pos, attacker: Color, occupied: Bitboard): Bitboard =
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
  def sliderBlockers(us: Color): Bitboard =
    kingPosOf(us).fold(Bitboard.empty) { ourKing =>
      val snipers = byColor(!us) & (
        ourKing.rookAttacks(Bitboard.empty) & (rooks ^ queens) |
          ourKing.bishopAttacks(Bitboard.empty) & (bishops ^ queens)
      )
      val bs = for
        sniper <- snipers.occupiedSquares
        between = Bitboard.between(ourKing, sniper) & occupied
        if !between.moreThanOne
      yield between

      bs.fold(Bitboard.empty)((a, b) => a | b)
    }

  def discard(s: Pos): Board =
    discard(s.bb)

  def discard(mask: Bitboard): Board =
    val notMask = ~mask
    copy(
      pawns = pawns & notMask,
      knights = knights & notMask,
      bishops = bishops & notMask,
      rooks = rooks & notMask,
      queens = queens & notMask,
      kings = kings & notMask,
      white = white & notMask,
      black = black & notMask,
      occupied & notMask
    )

  def roles: Role => Bitboard =
    case Pawn   => pawns
    case Knight => knights
    case Bishop => bishops
    case Rook   => rooks
    case Queen  => queens
    case King   => kings

  // put a piece to an empty square
  def put(piece: Piece, at: Pos): Option[Board] =
    !isOccupied(at) option putOrReplace(piece, at)

  // put a piece to an occupied square
  def replace(piece: Piece, at: Pos): Option[Board] =
    isOccupied(at) option putOrReplace(piece, at)

  // put a piece into the board
  def putOrReplace(s: Pos, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bb
    b.copy(
      pawns = if role == Pawn then b.pawns | m else b.pawns,
      knights = if role == Knight then b.knights | m else b.knights,
      bishops = if role == Bishop then b.bishops | m else b.bishops,
      rooks = if role == Rook then b.rooks | m else b.rooks,
      queens = if role == Queen then b.queens | m else b.queens,
      kings = if role == King then b.kings | m else b.kings,
      white = if color.white then b.white | m else b.white,
      black = if color.black then b.black | m else b.black,
      occupied = b.occupied ^ m
    )

  // put a piece into the board
  // remove the existing piece at that square if needed
  def putOrReplace(p: Piece, s: Pos): Board =
    putOrReplace(s, p.role, p.color)

  def take(at: Pos): Option[Board] =
    isOccupied(at) option discard(at)

  // move without capture
  def move(orig: Pos, dest: Pos): Option[Board] =
    if isOccupied(dest) then None
    else pieceAt(orig).map(discard(orig).putOrReplace(_, dest))

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for
      piece <- pieceAt(orig)
      takenPos = taking getOrElse dest
      if isOccupied(takenPos)
    yield discard(orig).discard(takenPos).putOrReplace(piece, dest)

  def promote(orig: Pos, dest: Pos, piece: Piece): Option[Board] =
    take(orig).map(_.putOrReplace(piece, dest))

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { c =>
    color(c).occupiedSquares.toSet
  }

  inline def isOccupied(inline p: Piece) =
    piece(p).nonEmpty

  // TODO remove unsafe get
  lazy val pieceMap: Map[Pos, Piece] =
    occupied.occupiedSquares.view.map(s => (s, pieceAt(s).get)).toMap

  def piecesOf(c: Color): Map[Pos, Piece] =
    pieceMap.filter((_, p) => p.color == c)

  def pieces: List[Piece] = pieces(occupied)

  def pieces(occupied: Bitboard): List[Piece] =
    occupied.occupiedSquares.flatMap(pieceAt)

  def color(c: Color): Bitboard = c.fold(white, black)

  def piece(p: Piece): Bitboard = color(p.color) & byRole(p.role)

object Board:
  val empty = Board(
    pawns = Bitboard.empty,
    knights = Bitboard.empty,
    bishops = Bitboard.empty,
    rooks = Bitboard.empty,
    queens = Bitboard.empty,
    kings = Bitboard.empty,
    white = Bitboard.empty,
    black = Bitboard.empty,
    occupied = Bitboard.empty
  )
  val standard = Board(
    pawns = Bitboard(0xff00000000ff00L),
    knights = Bitboard(0x4200000000000042L),
    bishops = Bitboard(0x2400000000000024L),
    rooks = Bitboard(0x8100000000000081L),
    queens = Bitboard(0x800000000000008L),
    kings = Bitboard(0x1000000000000010L),
    white = Bitboard(0xffffL),
    black = Bitboard(0xffff000000000000L),
    occupied = Bitboard(0xffff00000000ffffL)
  )

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

    pieces.foreach { (s, p) =>
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
    }
    Board(pawns, knights, bishops, rooks, queens, kings, white, black, occupied)
