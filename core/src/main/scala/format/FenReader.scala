package chess
package format

import cats.syntax.all.*
import chess.variant.Crazyhouse
import scalalib.zeros.given

import variant.{ Standard, Variant }
import variant.Crazyhouse.Pockets

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenReader:
  def read(variant: Variant, fen: FullFen): Option[Position] =
    val (fBoard, fColor, fCastling, fEnpassant) = fen.parts
    makeBoard(variant, fBoard).map { (board, crazyData) =>
      // We trust Fen's color to be correct, if there is no color we use the color of the king in check
      // If there is no king in check we use white
      val color = fColor.orElse(variant.checkColor(board)) | Color.White
      val position = new Position(
        board,
        History(
          unmovedRooks = variant.makeUnmovedRooks(board.rooks),
          crazyData = variant.crazyhouse.so(crazyData)
        ),
        variant,
        color
      )
      // todo verify unmovedRooks vs board.rooks
      val (castles, unmovedRooks) =
        if !variant.allowsCastling then (Castles.none -> UnmovedRooks.none)
        else
          fCastling.foldLeft(Castles.none -> UnmovedRooks.none):
            case ((c, r), ch) =>
              val color    = Color.fromWhite(ch.isUpper)
              val backRank = Bitboard.rank(color.backRank)
              // rooks that can be used for castling
              val rooks = position.rooks & position.byColor(color) & backRank
              {
                for
                  kingSquare <- (position.kingOf(color) & backRank).first
                  rookSquare <- ch.toLower match
                    case 'k'  => rooks.findLast(_ ?> kingSquare)
                    case 'q'  => rooks.find(_ ?< kingSquare)
                    case file => rooks.find(_.file.char == file)
                  side <- Side.kingRookSide(kingSquare, rookSquare)
                yield (c.add(color, side), r | rookSquare.bl)
              }.getOrElse(c -> r)

      import position.color.{ fifthRank, sixthRank, seventhRank }

      val enpassantMove = for
        square <- fEnpassant
        if square.rank == sixthRank
        orig = square.withRank(seventhRank)
        dest = square.withRank(fifthRank)
        if position.pieceAt(dest).exists(p => p.color != position.color && p.is(Pawn)) &&
          position.pieceAt(square.file, sixthRank).isEmpty &&
          position.pieceAt(orig).isEmpty
      yield Uci.Move(orig, dest)

      position
        .updateHistory: original =>
          val history = original.copy(
            lastMove = enpassantMove,
            positionHashes = PositionHash.empty,
            castles = castles,
            unmovedRooks = unmovedRooks
          )
          val checkCount = variant.threeCheck.so:
            val splitted = fen.value.split(' ')
            splitted
              .lift(4)
              .flatMap(readCheckCount)
              .orElse(splitted.lift(6).flatMap(readCheckCount))
          checkCount.foldLeft(history)(_.withCheckCount(_))
    }

  def read(fen: FullFen): Option[Position] = read(Standard, fen)

  def readWithMoveNumber(variant: Variant, fen: FullFen): Option[Position.AndFullMoveNumber] =
    read(variant, fen).map { sit =>
      val (halfMoveClock, fullMoveNumber) = readHalfMoveClockAndFullMoveNumber(fen)
      Position.AndFullMoveNumber(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(x => sit.updateHistory(_ => x)),
        fullMoveNumber | FullMoveNumber(1)
      )
    }

  def readWithMoveNumber(fen: FullFen): Option[Position.AndFullMoveNumber] =
    readWithMoveNumber(Standard, fen)

  def readHalfMoveClockAndFullMoveNumber(fen: FullFen): (Option[HalfMoveClock], Option[FullMoveNumber]) =
    val splitted = fen.value.split(' ').drop(4).dropWhile(_.contains('+')) // skip winboards 3check notation
    val halfMoveClock =
      HalfMoveClock
        .from(splitted.lift(0).flatMap(_.toIntOption))
        .map(_.atLeast(HalfMoveClock.initial).atMost(HalfMoveClock(100)))
    val fullMoveNumber = FullMoveNumber
      .from(splitted.lift(1).flatMap(_.toIntOption))
      .map(_.atLeast(FullMoveNumber.initial).atMost(FullMoveNumber(500)))
    (halfMoveClock, fullMoveNumber)

  def readPly(fen: FullFen): Option[Ply] =
    val (_, fullMoveNumber) = readHalfMoveClockAndFullMoveNumber(fen)
    fullMoveNumber.map(_.ply(fen.colorOrWhite))

  private def readCheckCount(str: String): Option[CheckCount] =
    str.toList match
      case '+' :: w :: '+' :: b :: Nil =>
        for
          white <- w.toString.toIntOption if white <= 3
          black <- b.toString.toIntOption if black <= 3
        yield CheckCount(black, white)
      case w :: '+' :: b :: Nil =>
        for
          white <- w.toString.toIntOption if white <= 3
          black <- b.toString.toIntOption if black <= 3
        yield CheckCount(3 - black, 3 - white)
      case _ => None

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(variant: Variant, fen: String): Option[(Board, Option[Crazyhouse.Data])] =
    val (position, pocketsStr) = fen.takeWhile(' ' !=) match
      case word if word.count('/' ==) == 8 =>
        val splitted = word.split('/')
        splitted.take(8).mkString("/") -> splitted.lift(8)
      case word if word.contains('[') && word.endsWith("]") =>
        word.span('[' !=) match
          case (position, pockets) => position -> pockets.stripPrefix("[").stripSuffix("]").some
      case word => word -> None
    if pocketsStr.isDefined && !variant.crazyhouse then None
    else
      makeBoardOptionWithCrazyPromoted(position, variant).map: (board, promoted) =>
        val pockets = pocketsStr.fold(Pockets.empty)(Pockets.apply)
        board -> Crazyhouse.Data(pockets, promoted).some

  private val numberSet = Set.from('1' to '8')

  type BoardWithCrazyPromoted = (Board, Bitboard)

  def makeBoardOptionWithCrazyPromoted(boardFen: String, variant: Variant): Option[BoardWithCrazyPromoted] =
    val (board, error) = makeBoardWithCrazyPromoted(boardFen, variant)
    error.fold(board.some)(_ => none)

  def makeBoardWithCrazyPromoted(
      boardFen: String,
      variant: Variant
  ): (BoardWithCrazyPromoted, Option[String]) =
    var promoted = Bitboard.empty
    var pawns    = Bitboard.empty
    var knights  = Bitboard.empty
    var bishops  = Bitboard.empty
    var rooks    = Bitboard.empty
    var queens   = Bitboard.empty
    var kings    = Bitboard.empty
    var white    = Bitboard.empty
    var black    = Bitboard.empty
    var occupied = Bitboard.empty

    inline def addPieceAt(p: Piece, s: Long) =
      occupied |= s
      p.role match
        case Pawn   => pawns |= s
        case Knight => knights |= s
        case Bishop => bishops |= s
        case Rook   => rooks |= s
        case Queen  => queens |= s
        case King   => kings |= s

      p.color match
        case Color.White => white |= s
        case Color.Black => black |= s

    var rank  = 7
    var file  = 0
    val iter  = boardFen.iterator.buffered
    var error = none[String]
    while iter.hasNext && error.isEmpty
    do
      if file >= 8 then
        file = 0
        rank -= 1
      if rank < 0 then error = Some("too many ranks")
      else
        iter.next match
          case '/' => // ignored, optional. Rank switch is automatic
          case ch if numberSet.contains(ch) =>
            file += (ch - '0')
            if file > 8 then error = Some(s"file = $file")
          case ch =>
            Piece
              .fromChar(ch)
              .match
                case Some(p) =>
                  val square = 1L << (file + 8 * rank)
                  addPieceAt(p, square)
                  if iter.headOption == Some('~') then
                    promoted |= square
                    iter.next
                case None => error = Some(s"invalid piece $ch")
            file += 1
    val board = Board(
      occupied = occupied,
      ByColor(
        white = white,
        black = black
      ),
      ByRole(
        pawn = pawns,
        knight = knights,
        bishop = bishops,
        rook = rooks,
        queen = queens,
        king = kings
      )
    )
    (board -> promoted, error)
