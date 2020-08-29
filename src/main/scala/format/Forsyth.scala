package chess
package format

import cats.implicits._
import variant.{ Standard, Variant }

/**
  * Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
object Forsyth {

  val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def <<@(variant: Variant, rawSource: String): Option[Situation] =
    read(rawSource) { fen =>
      makeBoard(variant, fen) map { board =>
        val splitted    = fen split ' '
        val colorOption = splitted lift 1 flatMap (_ lift 0) flatMap Color.apply
        val situation = colorOption match {
          case Some(color)             => Situation(board, color)
          case _ if board.check(Black) => Situation(board, Black) // user in check will move first
          case _                       => Situation(board, White)
        }
        splitted
          .lift(2)
          .fold(situation) { strCastles =>
            val (castles, unmovedRooks) = strCastles.foldLeft(Castles.none -> Set.empty[Pos]) {
              case ((c, r), ch) =>
                val color = Color(ch.isUpper)
                val rooks = board
                  .piecesOf(color)
                  .collect {
                    case (pos, piece) if piece.is(Rook) && pos.y0 == color.backrankY0 => pos
                  }
                  .toList
                  .sortBy(_.x0)
                (for {
                  kingPos <- board.kingPosOf(color)
                  rookPos <- (ch.toLower match {
                      case 'k'  => rooks.reverse.find(_ ?> kingPos)
                      case 'q'  => rooks.find(_ ?< kingPos)
                      case file => rooks.find(_.file == file.toString)
                    })
                  side <- Side.kingRookSide(kingPos, rookPos)
                } yield (c.add(color, side), r + rookPos)).getOrElse((c, r))
            }

            val fifthRank   = if (situation.color == White) 4 else 3
            val sixthRank   = if (situation.color == White) 5 else 2
            val seventhRank = if (situation.color == White) 6 else 1
            val lastMove = for {
              pos <- splitted lift 3 flatMap Pos.posAt
              if pos.y0 == sixthRank
              orig <- Pos.posAt0(pos.x0, seventhRank)
              dest <- Pos.posAt0(pos.x0, fifthRank) filter { d =>
                situation.board(d).contains(Piece(!situation.color, Pawn)) &&
                Pos.posAt0(pos.x0, sixthRank).flatMap(situation.board.apply).isEmpty &&
                situation.board(orig).isEmpty
              }
            } yield Uci.Move(orig, dest)

            situation withHistory {
              val history = History(
                lastMove = lastMove,
                positionHashes = Array.empty,
                castles = castles,
                unmovedRooks = UnmovedRooks(unmovedRooks)
              )
              val checkCount =
                splitted
                  .lift(4)
                  .flatMap(makeCheckCount)
                  .orElse(splitted.lift(6).flatMap(makeCheckCount))
              checkCount.fold(history)(history.withCheckCount)
            }
          } fixCastles
      }
    }

  def <<(rawSource: String): Option[Situation] = <<@(Standard, rawSource)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - (if (situation.color.white) 2 else 1)
  }

  def <<<@(variant: Variant, rawSource: String): Option[SituationPlus] =
    read(rawSource) { source =>
      <<@(variant, source) map { sit =>
        val splitted       = source.split(' ').drop(4).dropWhile(_.contains('+'))
        val fullMoveNumber = splitted lift 1 flatMap (_.toIntOption) map (_ max 1 min 500)
        val halfMoveClock  = splitted lift 0 flatMap (_.toIntOption) map (_ max 0 min 100)
        SituationPlus(
          halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
          fullMoveNumber | 1
        )
      }
    }

  def <<<(rawSource: String): Option[SituationPlus] = <<<@(Standard, rawSource)

  def makeCheckCount(str: String): Option[CheckCount] =
    str.toList match {
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
    }

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(variant: Variant, rawSource: String): Option[Board] =
    read(rawSource) { fen =>
      val (position, pockets) = fen.takeWhile(' ' !=) match {
        case word if word.count('/' ==) == 8 =>
          val splitted = word.split('/')
          splitted.take(8).mkString("/") -> splitted.lift(8)
        case word if word.contains('[') && word.endsWith("]") =>
          word.span('[' !=) match {
            case (position, pockets) => position -> pockets.stripPrefix("[").stripSuffix("]").some
          }
        case word => word -> None
      }
      makePiecesWithCrazyPromoted0(position.toList, 0, 7) map {
        case (pieces, promoted) =>
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
    }

  private def makePiecesWithCrazyPromoted0(
      chars: List[Char],
      x0: Int,
      y0: Int
  ): Option[(List[(Pos, Piece)], Set[Pos])] =
    chars match {
      case Nil                               => Option((Nil, Set.empty))
      case '/' :: rest                       => makePiecesWithCrazyPromoted0(rest, 0, y0 - 1)
      case c :: rest if '1' <= c && c <= '9' => makePiecesWithCrazyPromoted0(rest, x0 + (c - '0').toInt, y0) // TODO: <= 9?
      case c :: '~' :: rest =>
        for {
          pos                        <- Pos.posAt0(x0, y0)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted0(rest, x0 + 1, y0)
        } yield (pos -> piece :: nextPieces, nextPromoted + pos)
      case c :: rest =>
        for {
          pos                        <- Pos.posAt0(x0, y0)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted0(rest, x0 + 1, y0)
        } yield (pos -> piece :: nextPieces, nextPromoted)
    }

  def >>(situation: Situation): String = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): String =
    parsed match {
      case SituationPlus(situation, _) => >>(Game(situation, turns = parsed.turns))
    }

  def >>(game: Game): String = {
    List(
      exportBoard(game.board) + exportCrazyPocket(game.board),
      game.player.letter,
      exportCastles(game.board),
      game.situation.enPassantSquare.map(_.toString).getOrElse("-"),
      game.halfMoveClock,
      game.fullMoveNumber
    ) ::: {
      if (game.board.variant == variant.ThreeCheck) List(exportCheckCount(game.board))
      else List()
    }
  } mkString " "

  def exportStandardPositionTurnCastlingEp(situation: Situation): String =
    List(
      exportBoard(situation.board),
      situation.color.letter,
      exportCastles(situation.board),
      situation.enPassantSquare.map(_.toString).getOrElse("-")
    ) mkString " "

  private def exportCheckCount(board: Board) =
    board.history.checkCount match {
      case CheckCount(white, black) => s"+$black+$white"
    }

  private def exportCrazyPocket(board: Board) =
    board.crazyData match {
      case Some(variant.Crazyhouse.Data(pockets, _)) =>
        "/" +
          pockets.white.roles.map(_.forsythUpper).mkString +
          pockets.black.roles.map(_.forsyth).mkString
      case _ => ""
    }

  implicit private val posOrdering = Ordering.by[Pos, Int](_.x0)

  private[chess] def exportCastles(board: Board): String = {

    lazy val wr = board.pieces.collect {
      case (pos, piece) if pos.y0 == White.backrankY0 && piece == White.rook => pos
    }
    lazy val br = board.pieces.collect {
      case (pos, piece) if pos.y0 == Black.backrankY0 && piece == Black.rook => pos
    }

    lazy val wur = board.unmovedRooks.pos.filter(_.y0 == White.backrankY0)
    lazy val bur = board.unmovedRooks.pos.filter(_.y0 == Black.backrankY0)

    {
      // castling rights with inner rooks are represented by their file name
      (if (board.castles.whiteKingSide && wr.nonEmpty && wur.nonEmpty)
         (if (wur contains wr.max) "K" else wur.max.file.toUpperCase)
       else "") +
        (if (board.castles.whiteQueenSide && wr.nonEmpty && wur.nonEmpty)
           (if (wur contains wr.min) "Q" else wur.min.file.toUpperCase)
         else "") +
        (if (board.castles.blackKingSide && br.nonEmpty && bur.nonEmpty)
           (if (bur contains br.max) "k" else bur.max.file)
         else "") +
        (if (board.castles.blackQueenSide && br.nonEmpty && bur.nonEmpty)
           (if (bur contains br.min) "q" else bur.min.file)
         else "")
    } match {
      case "" => "-"
      case n  => n
    }
  }

  def exportBoard(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y0 <- 7 to 0 by -1) {
      empty = 0
      for (x0 <- 0 to 7) {
        board.apply0(x0, y0) match {
          case None => empty = empty + 1
          case Some(piece) =>
            if (empty == 0) fen append piece.forsyth.toString
            else {
              fen append (empty.toString + piece.forsyth)
              empty = 0
            }
            if (
              piece.role != Pawn && board.crazyData.fold(false)(_.promoted.exists { p =>
                p.x0 == x0 && p.y0 == y0
              })
            ) fen append '~'
        }
      }
      if (empty > 0) fen append empty
      if (y0 > 0) fen append '/'
    }
    fen.toString
  }

  def boardAndColor(situation: Situation): String =
    boardAndColor(situation.board, situation.color)

  def boardAndColor(board: Board, turnColor: Color): String =
    s"${exportBoard(board)} ${turnColor.letter}"

  def getFullMove(rawSource: String): Option[Int] =
    read(rawSource) {
      _.split(' ').lift(5).flatMap(_.toIntOption)
    }

  def getColor(rawSource: String): Option[Color] =
    read(rawSource) { fen =>
      fen.split(' ').lift(1) flatMap (_.headOption) flatMap Color.apply
    }

  def getPly(rawSource: String): Option[Int] =
    read(rawSource) { fen =>
      getFullMove(fen) map { fullMove =>
        fullMove * 2 - (if (getColor(fen).exists(_.white)) 2 else 1)
      }
    }

  private def read[A](source: String)(f: String => A): A = f(source.replace("_", " ").trim)
}
