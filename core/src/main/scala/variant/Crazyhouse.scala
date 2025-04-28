package chess
package variant

import chess.format.{ FullFen, Uci }
import monocle.syntax.all.*

import bitboard.Bitboard

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

  def pieces = Standard.pieces

  override val initialFen = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1")

  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)

  override def valid(situation: Board, strict: Boolean) =
    Color.all.forall(validSide(situation, false)) &&
      (!strict ||
        (situation.board.byRole(Pawn).count <= 16
          && situation.board.nbPieces <= 32
          && Standard.hasValidCheckers(situation)))

  private def canDropPawnOn(square: Square) = square.rank != Rank.First && square.rank != Rank.Eighth

  override def drop(situation: Situation, role: Role, square: Square): Either[ErrorStr, Drop] =
    for
      d1 <- situation.board.crazyData.toRight(ErrorStr("Board has no crazyhouse data"))
      _  <- Either.cond((role != Pawn || canDropPawnOn(square)), d1, ErrorStr(s"Can't drop $role on $square"))
      piece = Piece(situation.color, role)
      d2 <- d1.drop(piece).toRight(ErrorStr(s"No $piece to drop on $square"))
      b1 <- situation.board
        .place(piece, square)
        .toRight(ErrorStr(s"Can't drop $role on $square, it's occupied"))
      _ <- Either.cond(
        kingThreatened(b1.board, situation.color).no,
        b1,
        ErrorStr(s"Dropping $role on $square doesn't uncheck the king")
      )
    yield Drop(
      piece = piece,
      square = square,
      situationBefore = situation,
      after = b1.withCrazyData(d2)
    )

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match
      case Uci.Move(orig, dest, promOption) =>
        board.crazyData.fold(board) { data =>
          val d1 = capture.fold(data) { data.store(_, dest) }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1.promote(dest)
          }
          board.withCrazyData(d2)
        }
      case _ => board

  private def canDropStuff(situation: Board) =
    situation.crazyData.exists { (data: Data) =>
      val pocket = data.pockets(situation.color)
      pocket.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || pocket.hasNonPawn
        }
      }
    }

  override def staleMate(situation: Board) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Board) =
    super.checkmate(situation) && !canDropStuff(situation)

  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(situation: Board) = false
  override def isInsufficientMaterial(board: Board)              = false

  // if the king is not in check, all drops are possible, we just return None
  // king is in single check, we return the squares between the king and the checker
  // king is in double check, no drop is possible
  def possibleDrops(situation: Board): Option[List[Square]] =
    situation.ourKing.flatMap(king =>
      val checkers = situation.board.attackers(king, !situation.color)
      if checkers.moreThanOne then Some(Nil)
      else checkers.first.map(Bitboard.between(king, _).squares)
    )

  // all legal moves and drops
  // this function is used in perfts only
  def legalMoves(situation: Situation): List[MoveOrDrop] =
    legalDrops(situation) ::: situation.legalMoves.filterNot(m =>
      m.castle.exists(c => c.isStandard && m.dest != c.rook)
    )

  // if the king is not in check, all empty squares are possible drop
  // king is in single check, return the squares between the king and the checker
  // king is in double check, no drop is possible
  // this function is used in perfts only
  private def legalDropSquares(situation: Situation): Bitboard =
    situation.ourKing
      .map(king =>
        val checkers = situation.board.board.attackers(king, !situation.color)
        if checkers.isEmpty then ~situation.board.occupied
        else checkers.singleSquare.map(Bitboard.between(king, _)).getOrElse(Bitboard.empty)
      )
      .getOrElse(Bitboard.empty)

  // generate all legal drops
  // this function is used in perfts only
  private def legalDrops(situation: Situation): List[Drop] =
    val targets = legalDropSquares(situation)
    if targets.isEmpty then Nil
    else
      situation.board.crazyData.fold(Nil): data =>
        val pocket = data.pockets(situation.color)
        for
          role <- List(Pawn, Knight, Bishop, Rook, Queen)
          if pocket.contains(role)
          to <- if role == Pawn then targets & ~Bitboard.firstRank & ~Bitboard.lastRank else targets
          piece = Piece(situation.color, role)
          after <- situation.board.place(piece, to)
          d2    <- data.drop(piece)
        yield Drop(piece, to, situation, after.withCrazyData(d2))

  type Pockets = ByColor[Pocket]

  case class Data(
      pockets: Pockets,
      // in crazyhouse, a promoted piece becomes a pawn
      // when captured and put in the pocket.
      // there we need to remember which pieces are issued from promotions.
      // we do that by tracking their positions on the board.
      promoted: Bitboard
  ):

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
    val init = Data(ByColor.fill(Pocket.empty), Bitboard.empty)

  extension (pockets: Pockets)

    def take(piece: Piece): Option[Pockets] =
      pockets.update(piece.color, _.take(piece.role))

    def store(piece: Piece): Pockets =
      pockets.update(!piece.color, _.store(piece.role))

    def forsyth = pockets.reduce(_.forsythUpper + _.forsyth)

  case class Pocket(pawn: Int, knight: Int, bishop: Int, rook: Int, queen: Int):

    def forsythUpper = forsyth.toUpperCase
    def forsyth: String = forsyth(pawn, 'p') + forsyth(knight, 'n') +
      forsyth(bishop, 'b') + forsyth(rook, 'r') + forsyth(queen, 'q')

    def forsyth(role: Int, char: Char) = List.fill(role)(char).mkString

    def size       = pawn + knight + bishop + rook + queen
    def isEmpty    = size == 0
    def nonEmpty   = size > 0
    def hasNonPawn = knight + bishop + rook + queen > 0

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
      f(Pawn, pawn)
      f(Knight, knight)
      f(Bishop, bishop)
      f(Rook, rook)
      f(Queen, queen)

  object Pocket:
    val empty = Pocket(0, 0, 0, 0, 0)

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
