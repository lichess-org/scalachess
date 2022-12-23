package chess
package bitboard

import cats.syntax.all.*

import Bitboard.*

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
  def isOccupied(s: Pos): Boolean = occupied.contains(s.value)

  def sliders = bishops ^ rooks ^ queens

  val byColor = Color.Map(white, black)

  def roleAt(s: Pos): Option[Role] =
    if pawns.contains(s.value) then Some(Pawn)
    else if knights.contains(s.value) then Some(Knight)
    else if bishops.contains(s.value) then Some(Bishop)
    else if rooks.contains(s.value) then Some(Rook)
    else if queens.contains(s.value) then Some(Queen)
    else if kings.contains(s.value) then Some(King)
    else None

  def colorAt(s: Pos): Option[Color] =
    if white.contains(s.value) then Some(Color.White)
    else if black.contains(s.value) then Some(Color.Black)
    else None

  def pieceAt(s: Pos): Option[Piece] =
    (colorAt(s), roleAt(s)).mapN(Piece.apply)

  // TODO returns Option[Boolean]
  // None in case of no king
  def whiteAt(s: Pos): Boolean =
    colorAt(s).contains(Color.White)

  def blackAt(s: Pos): Boolean =
    colorAt(s).contains(Color.Black)

  def king(color: Color): Option[Pos] =
    (kings & byColor(color)).lsb

  def attacksTo(s: Pos, attacker: Color): Bitboard =
    attacksTo(s, attacker, occupied)

  def attacksTo(s: Pos, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.kingAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
    )

  // return true if the king with color is in check
  // return false in case of no king
  def isCheck(color: Color): Boolean =
    king(color).exists(k => attacksTo(k, !color) != empty)

  /** Find all blockers between the king and attacking sliders First we find all snipers (all potential sliders which
    * can attack the king) Then we loop over those snipers if there is only one blockers between the king and the sniper
    * we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(us: Color): Bitboard =
    king(us).fold(Bitboard.empty) { ourKing =>
      val snipers = byColor(!us) & (
        ourKing.rookAttacks(Bitboard.empty) & (rooks ^ queens) |
          ourKing.bishopAttacks(Bitboard.empty) & (bishops ^ queens)
      )
      val bs = for
        sniper <- snipers.occupiedSquares
        between = Bitboard.between(ourKing, sniper) & occupied
        if !between.moreThanOne
      yield between

      bs.fold(0L.bb)((a, b) => a | b)
    }

  // TODO move: Board => Board
  // We can implement as PieceMap => PieceMap
  def play(color: Color): Move => Board =
    case Move.Normal(from, to, role, _)    => discard(from).put(to, role, color)
    case Move.Promotion(from, to, role, _) => discard(from).put(to, role, color)
    case Move.EnPassant(from, to)          => discard(from).discard(to.combine(from)).put(to, Pawn, color)
    case Move.Castle(from, to) =>
      val rookTo = (if to < from then Pos.D1 else Pos.F1).combine(to)
      val kingTo = (if to < from then Pos.C1 else Pos.G1).combine(to)
      discard(from).discard(to).put(rookTo, Rook, color).put(kingTo, King, color)

  // todo more efficient
  def discard(s: Pos): Board =
    pieceAt(s).fold(this) { p =>
      val m = s.bitboard
      copy(
        pawns = updateRole(m, Pawn)(p.role),
        knights = updateRole(m, Knight)(p.role),
        bishops = updateRole(m, Bishop)(p.role),
        rooks = updateRole(m, Rook)(p.role),
        queens = updateRole(m, Queen)(p.role),
        kings = updateRole(m, King)(p.role),
        white = updateColor(m, Color.White)(p.color),
        black = updateColor(m, Color.Black)(p.color),
        occupied ^ m
      )
    }

  def updateRole(mask: Bitboard, role: Role): Role => Bitboard =
    case Pawn if role == Pawn     => pawns ^ mask
    case Knight if role == Knight => knights ^ mask
    case Bishop if role == Bishop => bishops ^ mask
    case Rook if role == Rook     => rooks ^ mask
    case Queen if role == Queen   => queens ^ mask
    case King if role == King     => kings ^ mask
    case _                        => roles(role)

  def roles: Role => Bitboard =
    case Pawn   => pawns
    case Knight => knights
    case Bishop => bishops
    case Rook   => rooks
    case Queen  => queens
    case King   => kings

  def updateColor(mask: Bitboard, color: Color): Color => Bitboard =
    case Color.White if color == Color.White => white ^ mask
    case Color.Black if color == Color.Black => black ^ mask
    case _                                   => colors(color)

  def colors: Color => Bitboard =
    case Color.White => white
    case Color.Black => black

  def put(s: Pos, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bitboard
    b.copy(
      pawns = b.updateRole(m, Pawn)(role),
      knights = b.updateRole(m, Knight)(role),
      bishops = b.updateRole(m, Bishop)(role),
      rooks = b.updateRole(m, Rook)(role),
      queens = b.updateRole(m, Queen)(role),
      kings = b.updateRole(m, King)(role),
      white = b.updateColor(m, Color.White)(color),
      black = b.updateColor(m, Color.Black)(color),
      occupied = b.occupied ^ m
    )

  def put(s: Pos, p: Piece): Board =
    put(s, p.role, p.color)

  // TODO remove unsafe get
  // we believe in the integrity of bitboard
  // tests pieceMap . fromMap = identity
  def pieceMap: Map[Pos, Piece] =
    occupied.occupiedSquares.map(s => (s, pieceAt(s).get)).toMap

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

  def fromMap(pieces: Map[Pos, Piece]): Board =
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
      val position = s.bitboard
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
