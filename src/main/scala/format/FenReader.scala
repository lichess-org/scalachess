package chess
package format

import cats.syntax.all.*
import variant.{ Standard, Variant }
import variant.Crazyhouse.Pockets
import ornicar.scalalib.zeros.given
import bitboard.Bitboard
import bitboard.Board as BBoard

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenReader:
  def read(variant: Variant, fen: EpdFen): Option[Situation] =
    val (fBoard, fColor, fCastling, fEnpassant) = fen.parts
    makeBoard(variant, fBoard) map { board =>
      // We trust Fen's color to be correct, if there is no color we use the color of the king in check
      // If there is no king in check we use white
      val color     = fColor.orElse(board.checkColor) | Color.White
      val situation = Situation(board, color)
      // todo verify unmovedRooks vs board.rooks
      val (castles, unmovedRooks) =
        if !variant.allowsCastling then (Castles.none -> UnmovedRooks.none)
        else
          fCastling.foldLeft(Castles.none -> UnmovedRooks.none):
            case ((c, r), ch) =>
              val color    = Color.fromWhite(ch.isUpper)
              val backRank = Bitboard.rank(color.backRank)
              // rooks that can be used for castling
              val rooks = board.rooks & board(color) & backRank
              {
                for
                  kingSquare <- (board.kingOf(color) & backRank).first
                  rookSquare <- ch.toLower match
                    case 'k'  => rooks.findLast(_ ?> kingSquare)
                    case 'q'  => rooks.find(_ ?< kingSquare)
                    case file => rooks.find(_.file.char == file)
                  side <- Side.kingRookSide(kingSquare, rookSquare)
                yield (c.add(color, side), r | rookSquare.bl)
              }.getOrElse(c -> r)

      import situation.color.{ fifthRank, sixthRank, seventhRank }

      val enpassantMove = for
        square <- fEnpassant
        if square.rank == sixthRank
        orig = square `withRank` seventhRank
        dest = square `withRank` fifthRank
        if situation.board(dest).contains(Piece(!situation.color, Pawn)) &&
          situation.board(square.file, sixthRank).isEmpty &&
          situation.board(orig).isEmpty
      yield Uci.Move(orig, dest)

      situation withHistory:
        val history = History(
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
        checkCount.foldLeft(history)(_ `withCheckCount` _)
    }

  def read(fen: EpdFen): Option[Situation] = read(Standard, fen)

  def readWithMoveNumber(variant: Variant, fen: EpdFen): Option[Situation.AndFullMoveNumber] =
    read(variant, fen) map { sit =>
      val (halfMoveClock, fullMoveNumber) = readHalfMoveClockAndFullMoveNumber(fen)
      Situation.AndFullMoveNumber(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
        fullMoveNumber | FullMoveNumber(1)
      )
    }

  def readWithMoveNumber(fen: EpdFen): Option[Situation.AndFullMoveNumber] =
    readWithMoveNumber(Standard, fen)

  def readHalfMoveClockAndFullMoveNumber(fen: EpdFen): (Option[HalfMoveClock], Option[FullMoveNumber]) =
    val splitted = fen.value.split(' ').drop(4).dropWhile(_.contains('+')) // skip winboards 3check notation
    val halfMoveClock =
      HalfMoveClock
        .from(splitted.lift(0).flatMap(_.toIntOption))
        .map(_ atLeast HalfMoveClock.initial atMost 100)
    val fullMoveNumber = FullMoveNumber
      .from(splitted.lift(1).flatMap(_.toIntOption))
      .map(_ atLeast FullMoveNumber.initial atMost 500)
    (halfMoveClock, fullMoveNumber)

  def readPly(fen: EpdFen): Option[Ply] =
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
  def makeBoard(variant: Variant, fen: String): Option[Board] =
    val (position, pockets) = fen.takeWhile(' ' !=) match
      case word if word.count('/' ==) == 8 =>
        val splitted = word.split('/')
        splitted.take(8).mkString("/") -> splitted.lift(8)
      case word if word.contains('[') && word.endsWith("]") =>
        word.span('[' !=) match
          case (position, pockets) => position -> pockets.stripPrefix("[").stripSuffix("]").some
      case word => word -> None
    if pockets.isDefined && !variant.crazyhouse then None
    else
      makeBoardOptionWithCrazyPromoted(position, variant).map: board =>
        pockets.fold(board): str =>
          board.withCrazyData(_.copy(pockets = Pockets(str.flatMap(Piece.fromChar))))

  private val numberSet = Set.from('1' to '8')

  def makeBoardOptionWithCrazyPromoted(boardFen: String, variant: Variant): Option[Board] =
    val (board, error) = makeBoardWithCrazyPromoted(boardFen, variant)
    error.fold(board.some)(_ => none)

  def makeBoardWithCrazyPromoted(boardFen: String, variant: Variant): (Board, Option[String]) =
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
    val bboard = BBoard(
      occupied = occupied,
      white = white,
      black = black,
      pawns = pawns,
      knights = knights,
      bishops = bishops,
      rooks = rooks,
      queens = queens,
      kings = kings
    )
    val board = Board(bboard, variant)
    if promoted.isEmpty
    then (board, error)
    else (board.withCrazyData(_.copy(promoted = promoted)), error)
