package chess
package variant

import chess.format.FullFen
import monocle.syntax.all.*

case object Crazyhouse
    extends Variant(
      id = Variant.Id(10),
      key = Variant.LilaKey("crazyhouse"),
      uciKey = Variant.UciKey("crazyhouse"),
      name = "Crazyhouse",
      shortName = "Crazy",
      title = "Captured pieces can be dropped back on the board instead of moving a piece.",
      standardInitialPosition = true
    ):

  override val pieces: Map[Square, Piece] = Standard.pieces

  override val initialFen: FullFen = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1")

  override def validMoves(position: Position): List[Move] =
    Standard.validMoves(position).map(updateCrazyData)

  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).view.filter(kingSafety).map(updateCrazyData).toList

  override def valid(position: Position, strict: Boolean): Boolean =
    Color.all.forall(validSide(position, false)) &&
      (!strict ||
        (position.byRole(Pawn).count <= 16
          && position.nbPieces <= 32
          && Standard.hasValidCheckers(position)))

  override def drop(position: Position, role: Role, square: Square): Either[ErrorStr, Drop] =
    for
      d1 <- position.crazyData.toRight(ErrorStr("Position has no crazyhouse data"))
      _  <- Either.cond((role != Pawn || canDropPawnOn(square)), d1, ErrorStr(s"Can't drop $role on $square"))
      piece = Piece(position.color, role)
      d2 <- d1.drop(piece).toRight(ErrorStr(s"No $piece to drop on $square"))
      b1 <- position.board
        .put(piece, square)
        .toRight(ErrorStr(s"Can't drop $role on $square, it's occupied"))
      b2 = position.withBoard(b1).withCrazyData(d2)
      _ <- Either.cond(
        kingThreatened(b2.board, position.color).no,
        b2,
        ErrorStr(s"Dropping $role on $square doesn't uncheck the king")
      )
    yield Drop(piece = piece, square = square, before = position, afterWithoutHistory = b2)

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def staleMate(position: Position): Boolean =
    super.staleMate(position) && !canDropStuff(position)

  override def checkmate(position: Position): Boolean =
    super.checkmate(position) && !canDropStuff(position)

  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(position: Position): Boolean = false
  override def isInsufficientMaterial(position: Position): Boolean          = false

  // if the king is not in check, all drops are possible, we just return None
  // king is in single check, we return the squares between the king and the checker
  // king is in double check, no drop is possible
  private[chess] def possibleDrops(position: Position): Option[List[Square]] =
    position.ourKing.flatMap(king =>
      val checkers = position.attackers(king, !position.color)
      if checkers.moreThanOne then Some(Nil)
      else checkers.first.map(Bitboard.between(king, _).squares)
    )

  private def updateCrazyData(move: Move): Move =
    val after = move.afterWithoutHistory.crazyData.fold(move.afterWithoutHistory): data =>
      val d1 = move.capture.flatMap(move.before.pieceAt).fold(data)(data.store(_, move.dest))
      val d2 = move.promotion.fold(d1.move(move.orig, move.dest))(_ => d1.promote(move.dest))
      move.afterWithoutHistory.withCrazyData(d2)
    move.copy(afterWithoutHistory = after)

  private def canDropPawnOn(square: Square): Boolean =
    square.rank != Rank.First && square.rank != Rank.Eighth

  // all legal moves and drops
  // this function is used in perfts only
  def legalMoves(position: Position): List[MoveOrDrop] =
    legalDrops(position) ::: position.legalMoves.filterNot(m =>
      m.castle.exists(c => c.isStandard && m.dest != c.rook)
    )

  private def canDropStuff(position: Position): Boolean =
    position.crazyData.exists { (data: Data) =>
      val pocket = data.pockets(position.color)
      pocket.nonEmpty && possibleDrops(position).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || pocket.hasNonPawn
        }
      }
    }

  // if the king is not in check, all empty squares are possible drop
  // king is in single check, return the squares between the king and the checker
  // king is in double check, no drop is possible
  // this function is used in perfts only
  private def legalDropSquares(position: Position): Bitboard =
    position.ourKing
      .map(king =>
        val checkers = position.attackers(king, !position.color)
        if checkers.isEmpty then ~position.occupied
        else checkers.singleSquare.map(Bitboard.between(king, _)).getOrElse(Bitboard.empty)
      )
      .getOrElse(Bitboard.empty)

  // generate all legal drops
  // this function is used in perfts only
  private def legalDrops(position: Position): List[Drop] =
    val targets = legalDropSquares(position)
    if targets.isEmpty then Nil
    else
      position.crazyData.fold(Nil): data =>
        val pocket = data.pockets(position.color)
        for
          role <- List(Pawn, Knight, Bishop, Rook, Queen)
          if pocket.contains(role)
          to <- if role == Pawn then targets & ~Bitboard.firstRank & ~Bitboard.lastRank else targets
          piece = Piece(position.color, role)
          after <- position.board.put(piece, to)
          d2    <- data.drop(piece)
        yield Drop(piece, to, position, position.withBoard(after).withCrazyData(d2))

  type Pockets = ByColor[Pocket]

  /**
  * in crazyhouse, a promoted piece becomes a pawn
  * when captured and put in the pocket.
  * there we need to remember which pieces are issued from promotions.
  * we do that by tracking their positions on the board.
  * */
  case class Data(pockets: Pockets, promoted: Bitboard):

    def drop(piece: Piece): Option[Data] =
      this.focus(_.pockets).modifyF(_.take(piece))

    def store(piece: Piece, from: Square): Data =
      val storedPiece = if promoted.contains(from) then piece.color.pawn else piece
      copy(
        pockets = pockets.store(storedPiece),
        promoted = promoted.remove(from)
      )

    def promote(square: Square): Data = copy(promoted = promoted.add(square))

    def move(orig: Square, dest: Square): Data =
      if promoted.contains(orig) then copy(promoted = promoted.move(orig, dest)) else this

    def isEmpty = pockets.forall(_.isEmpty)
    def size    = pockets.reduce(_.size + _.size)

  object Data:
    val init: Data = Data(Pockets.empty, Bitboard.empty)

  extension (pockets: Pockets)

    def take(piece: Piece): Option[Pockets] =
      pockets.update(piece.color, _.take(piece.role))

    def store(piece: Piece): Pockets =
      pockets.update(!piece.color, _.store(piece.role))

    def forsyth = pockets.reduce(_.forsythUpper + _.forsyth)

  case class Pocket(pawn: Int, knight: Int, bishop: Int, rook: Int, queen: Int):

    def forsythUpper: String = forsyth.toUpperCase
    def forsyth: String      = forsyth(pawn, 'p') + forsyth(knight, 'n') +
      forsyth(bishop, 'b') + forsyth(rook, 'r') + forsyth(queen, 'q')

    def forsyth(role: Int, char: Char): String = List.fill(role)(char).mkString

    def size: Int           = pawn + knight + bishop + rook + queen
    def isEmpty: Boolean    = size == 0
    def nonEmpty: Boolean   = size > 0
    def hasNonPawn: Boolean = knight + bishop + rook + queen > 0

    def contains(r: Role): Boolean = r match
      case Pawn   => pawn > 0
      case Knight => knight > 0
      case Bishop => bishop > 0
      case Rook   => rook > 0
      case Queen  => queen > 0
      case King   => false

    def apply(role: Role): Option[Int] =
      role match
        case Pawn   => Some(pawn)
        case Knight => Some(knight)
        case Bishop => Some(bishop)
        case Rook   => Some(rook)
        case Queen  => Some(queen)
        case King   => None

    def take(role: Role): Option[Pocket] =
      update(role, (x => Option.when(x > 0)(x - 1)))

    def store(role: Role): Pocket = update(role, _ + 1)

    def update(role: Role, f: Int => Int): Pocket = role match
      case Pawn   => copy(pawn = f(pawn))
      case Knight => copy(knight = f(knight))
      case Bishop => copy(bishop = f(bishop))
      case Rook   => copy(rook = f(rook))
      case Queen  => copy(queen = f(queen))
      case King   => this

    def update(role: Role, f: Int => Option[Int]): Option[Pocket] = role match
      case Pawn   => f(pawn).map(x => copy(pawn = x))
      case Knight => f(knight).map(x => copy(knight = x))
      case Bishop => f(bishop).map(x => copy(bishop = x))
      case Rook   => f(rook).map(x => copy(rook = x))
      case Queen  => f(queen).map(x => copy(queen = x))
      case King   => None

    def flatMap[B](f: (Role, Int) => IterableOnce[B]): List[B] =
      List(f(Pawn, pawn), f(Knight, knight), f(Bishop, bishop), f(Rook, rook), f(Queen, queen)).flatten

    def map[B](f: (Role, Int) => B): List[B] =
      List(f(Pawn, pawn), f(Knight, knight), f(Bishop, bishop), f(Rook, rook), f(Queen, queen))

    def foreach[U](f: (Role, Int) => U): Unit =
      f(Pawn, pawn): Unit
      f(Knight, knight): Unit
      f(Bishop, bishop): Unit
      f(Rook, rook): Unit
      f(Queen, queen): Unit

  object Pocket:
    val empty: Pocket = Pocket(0, 0, 0, 0, 0)

    def apply(roles: Seq[Role]): Pocket =
      var pawn   = 0
      var knight = 0
      var bishop = 0
      var rook   = 0
      var queen  = 0
      roles.foreach:
        case Pawn   => pawn += 1
        case Knight => knight += 1
        case Bishop => bishop += 1
        case Rook   => rook += 1
        case Queen  => queen += 1
        case King   =>
      Pocket(pawn, knight, bishop, rook, queen)

  object Pockets:
    inline def apply(pieces: Seq[Piece]): Pockets =
      var whitePawn   = 0
      var whiteKnight = 0
      var whiteBishop = 0
      var whiteRook   = 0
      var whiteQueen  = 0
      var blackPawn   = 0
      var blackKnight = 0
      var blackBishop = 0
      var blackRook   = 0
      var blackQueen  = 0
      pieces.foreach:
        case Piece(White, Pawn)   => whitePawn += 1
        case Piece(White, Knight) => whiteKnight += 1
        case Piece(White, Bishop) => whiteBishop += 1
        case Piece(White, Rook)   => whiteRook += 1
        case Piece(White, Queen)  => whiteQueen += 1
        case Piece(Black, Pawn)   => blackPawn += 1
        case Piece(Black, Knight) => blackKnight += 1
        case Piece(Black, Bishop) => blackBishop += 1
        case Piece(Black, Rook)   => blackRook += 1
        case Piece(Black, Queen)  => blackQueen += 1
        case Piece(_, King)       =>
      ByColor(
        Pocket(whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen),
        Pocket(blackPawn, blackKnight, blackBishop, blackRook, blackQueen)
      )

    inline def apply(str: String): Pockets =
      apply(str.flatMap(Piece.fromChar))

    val empty: Pockets = ByColor.fill(Pocket.empty)
