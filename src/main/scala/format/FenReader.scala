package chess
package format

import cats.implicits.*
import variant.{ Standard, Variant }
import cats.kernel.Monoid
import ornicar.scalalib.zeros.given_Zero_Option
import bitboard.Bitboard
import bitboard.Bitboard.bitboard

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenReader:
  def read(variant: Variant, fen: EpdFen): Option[Situation] =
    makeBoard(variant, fen) map { board =>
      // why it is different when the variant is Atomic?
      val situation = Situation(board, if variant.atomic then fen.color else board.checkColor | fen.color)
      // todo verify unmovedRooks vs board.rooks
      val (castles, unmovedRooks) = fen.castling.foldLeft(Castles.none -> UnmovedRooks.empty) {
        case ((c, r), ch) =>
          val color = Color.fromWhite(ch.isUpper)
          // todo (board.rooks & board.us).occupiedSquares or fold
          val rooks: List[Pos] = board
            .piecesOf(color)
            .collect {
              case (pos, piece) if piece.is(Rook) && pos.rank == color.backRank => pos
            }
            .toList
            .sortBy(_.file.value)
          (for {
            kingPos <- board.kingPosOf(color).headOption
            rookPos <- (ch.toLower match {
              case 'k'  => rooks.reverse.find(_ ?> kingPos)
              case 'q'  => rooks.find(_ ?< kingPos)
              case file => rooks.find(_.file.char == file)
            })
            side <- Side.kingRookSide(kingPos, rookPos)
          } yield (c.add(color, side), r | rookPos.bitboard)).getOrElse((c, r))
      }

      val fifthRank   = if (situation.color.white) Rank.Fifth else Rank.Fourth
      val sixthRank   = if (situation.color.white) Rank.Sixth else Rank.Third
      val seventhRank = if (situation.color.white) Rank.Seventh else Rank.Second

      val enpassantPos = fen.enpassant
      val enpassantMove = for {
        pos <- enpassantPos
        if pos.rank == sixthRank
        orig = Pos(pos.file, seventhRank)
        dest = Pos(pos.file, fifthRank)
        if situation.board(dest).contains(Piece(!situation.color, Pawn)) &&
          situation.board(pos.file, sixthRank).isEmpty &&
          situation.board(orig).isEmpty
      } yield Uci.Move(orig, dest)

      situation withHistory {
        val history = History(
          lastMove = enpassantMove,
          positionHashes = Monoid[PositionHash].empty,
          castles = castles,
          unmovedRooks = unmovedRooks
        )
        val checkCount = variant.threeCheck.?? {
          val splitted = fen.value split ' '
          splitted
            .lift(4)
            .flatMap(readCheckCount)
            .orElse(splitted.lift(6).flatMap(readCheckCount))
        }
        checkCount.foldLeft(history)(_ withCheckCount _)
      }
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
    val halfMoveClock  = splitted.lift(0).flatMap(_.toIntOption).map(_ max 0 min 100)
    val fullMoveNumber = splitted.lift(1).flatMap(_.toIntOption).map(_ max 1 min 500)
    (HalfMoveClock from halfMoveClock, FullMoveNumber from fullMoveNumber)

  def readPly(fen: EpdFen): Option[Ply] =
    val (_, fullMoveNumber) = readHalfMoveClockAndFullMoveNumber(fen)
    fullMoveNumber.map(_.ply(fen.color))

  private def readCheckCount(str: String): Option[CheckCount] =
    str.toList match
      case '+' :: w :: '+' :: b :: Nil =>
        for {
          white <- w.toString.toIntOption if white <= 3
          black <- b.toString.toIntOption if black <= 3
        } yield CheckCount(black, white)
      case w :: '+' :: b :: Nil =>
        for {
          white <- w.toString.toIntOption if white <= 3
          black <- b.toString.toIntOption if black <= 3
        } yield CheckCount(3 - black, 3 - white)
      case _ => None

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(variant: Variant, fen: EpdFen): Option[Board] =
    val (position, pockets) = fen.value.takeWhile(' ' !=) match
      case word if word.count('/' ==) == 8 =>
        val splitted = word.split('/')
        splitted.take(8).mkString("/") -> splitted.lift(8)
      case word if word.contains('[') && word.endsWith("]") =>
        word.span('[' !=) match
          case (position, pockets) => position -> pockets.stripPrefix("[").stripSuffix("]").some
      case word => word -> None
    if (pockets.isDefined && !variant.crazyhouse) None
    else
      makePiecesWithCrazyPromoted(position.toList, 0, 7) map { (pieces, promoted) =>
        val board = Board(pieces, variant = variant)
        if (promoted.isEmpty) board else board.withCrazyData(_.copy(promoted = promoted))
      } map { board =>
        pockets.fold(board) { str =>
          import chess.variant.Crazyhouse.{ Pocket, Pockets }
          val (white, black) = str.toList.flatMap(Piece.fromChar).partition(_ is White)
          board.withCrazyData(
            _.copy(
              pockets = Pockets(
                white = Pocket(white.map(_.role)),
                black = Pocket(black.map(_.role))
              )
            )
          )
        }
      }

  private def makePiecesWithCrazyPromoted(
      chars: List[Char],
      x: Int,
      y: Int
  ): Option[(List[(Pos, Piece)], Set[Pos])] =
    chars match
      case Nil                               => Option((Nil, Set.empty))
      case '/' :: rest                       => makePiecesWithCrazyPromoted(rest, 0, y - 1)
      case c :: rest if '1' <= c && c <= '8' => makePiecesWithCrazyPromoted(rest, x + (c - '0').toInt, y)
      case c :: '~' :: rest =>
        for {
          pos                        <- Pos.at(x, y)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, x + 1, y)
        } yield (pos -> piece :: nextPieces, nextPromoted + pos)
      case c :: rest =>
        for {
          pos                        <- Pos.at(x, y)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, x + 1, y)
        } yield (pos -> piece :: nextPieces, nextPromoted)
