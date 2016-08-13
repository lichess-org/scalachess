package chess
package format

import variant.{ Variant, Standard }

/**
 * Transform a game to standard Forsyth Edwards Notation
 * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 *
 * Crazyhouse & Threecheck extensions:
 * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
 */
object Forsyth {

  val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def <<@(variant: Variant, rawSource: String): Option[Situation] = read(rawSource) { fen =>
    makeBoard(variant, fen) map { board =>
      val splitted = fen split ' '
      val colorOption = splitted lift 1 flatMap (_ lift 0) flatMap Color.apply
      val situation = colorOption match {
        case Some(color)             => Situation(board, color)
        case _ if board.check(Black) => Situation(board, Black) // user in check will move first
        case _                       => Situation(board, White)
      }
      splitted.lift(2).fold(situation) { strCastles =>
        val (castles, unmovedRooks) = strCastles.foldLeft(Castles.none, Set.empty[Pos]) {
          case ((c, r), ch) =>
            val color = Color(ch.isUpper)
            val rooks = board.piecesOf(color).collect {
              case (pos, piece) if piece.is(Rook) && pos.y == color.backrankY => pos
            }.toList.sortBy(_.x)
            (for {
              kingPos <- board.kingPosOf(color)
              rookPos <- (ch.toLower match {
                case 'k'  => rooks.reverse.find(_.x > kingPos.x)
                case 'q'  => rooks.find(_.x < kingPos.x)
                case file => rooks.find(_.file == file.toString)
              })
              side <- Side.kingRookSide(kingPos, rookPos)
            } yield (c.add(color, side), r + rookPos)).getOrElse((c, r))
        }

        val fifthRank = if (situation.color == White) 5 else 4
        val sixthRank = if (situation.color == White) 6 else 3
        val seventhRank = if (situation.color == White) 7 else 2
        val lastMove = for {
          pos <- splitted lift 3 flatMap Pos.posAt
          if (pos.y == sixthRank)
          orig <- Pos.posAt(pos.x, seventhRank)
          dest <- Pos.posAt(pos.x, fifthRank)
          if (situation.board(dest).contains(Piece(!situation.color, Pawn)))
          if (Pos.posAt(pos.x, sixthRank).flatMap(situation.board.apply).isEmpty)
          if (situation.board(orig).isEmpty)
        } yield Uci.Move(orig, dest)

        situation withHistory {
          val history = History.make(lastMove, Array.empty, castles, unmovedRooks)
          (splitted lift 6 flatMap makeCheckCount).fold(history)(history.withCheckCount)
        }
      } fixCastles
    }
  }

  def <<(rawSource: String): Option[Situation] = <<@(Standard, rawSource)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - (if (situation.color.white) 2 else 1)
  }

  def <<<@(variant: Variant, rawSource: String): Option[SituationPlus] = read(rawSource) { source =>
    <<@(variant, source) map { sit =>
      val splitted = source split ' '
      val fullMoveNumber = splitted lift 5 flatMap parseIntOption map (_ max 1 min 500)
      val halfMoveClock = splitted lift 4 flatMap parseIntOption map (_ max 0 min 100)
      SituationPlus(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
        fullMoveNumber | 1)
    }
  }

  def <<<(rawSource: String): Option[SituationPlus] = <<<@(Standard, rawSource)

  def makeCheckCount(str: String): Option[CheckCount] = str.toList match {
    case '+' :: w :: '+' :: b :: Nil => for {
      white <- parseIntOption(w.toString) if white <= 3
      black <- parseIntOption(b.toString) if black <= 3
    } yield CheckCount(black, white)
    case _ => None
  }

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(variant: Variant, rawSource: String): Option[Board] = read(rawSource) { fen =>
    val (position, pockets) = fen.takeWhile(' '!=) match {
      case word if (word.count('/' ==) == 8) =>
        val splitted = word.split('/')
        splitted.take(8).mkString -> splitted.lift(8)
      case word => word -> None
    }
    {
      if (position contains '~') makePiecesWithCrazyPromoted(position.toList, Pos.A8)
      else makePieces(position.toList, Pos.A8) map { _ -> Set.empty[Pos] }
    } map {
      case (pieces, promoted) =>
        val board = Board(pieces, variant = variant)
        if (promoted.isEmpty) board else board.withCrazyData(_.copy(promoted = promoted))
    } map { board =>
      pockets.fold(board) { str =>
        import chess.variant.Crazyhouse.{ Pocket, Pockets }
        val (white, black) = str.toList.flatMap(Piece.fromChar).partition(_ is White)
        board.withCrazyData(_.copy(
          pockets = Pockets(
            white = Pocket(white.map(_.role)),
            black = Pocket(black.map(_.role)))))
      }
    }
  }

  private def makePieces(chars: List[Char], pos: Pos): Option[List[(Pos, Piece)]] = chars match {
    case Nil => Some(Nil)
    case c :: rest => c match {
      case n if n.toInt < 58 =>
        makePieces(rest,
          if (n.toInt > 48) tore(pos, n.toInt - 48) getOrElse pos
          else pos)
      case n => Role forsyth n.toLower map { role =>
        (pos, Piece(Color(n.isUpper), role)) :: {
          tore(pos, 1) flatMap { makePieces(rest, _) } getOrElse Nil
        }
      }
    }
  }

  private def makePiecesWithCrazyPromoted(chars: List[Char], pos: Pos): Option[(List[(Pos, Piece)], Set[Pos])] = chars match {
    case Nil => Some(Nil -> Set.empty)
    case c :: rest => c match {
      case '~' => pos match {
        case Pos.A1 => Some(Nil -> Set(Pos.H1)) // last piece is promoted
        case pos => for {
          prevPos <- tore(pos, -1)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, pos)
        } yield nextPieces -> (nextPromoted + prevPos)
      }
      case n if n.toInt < 58 =>
        makePiecesWithCrazyPromoted(rest,
          if (n.toInt > 48) tore(pos, n.toInt - 48) getOrElse pos
          else pos)
      case n => for {
        role <- Role forsyth n.toLower
        nextPos = tore(pos, 1) getOrElse Pos.A1
        (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, nextPos)
      } yield ((pos, Piece(Color(n.isUpper), role)) :: nextPieces) -> nextPromoted
    }
  }

  def >>(situation: Situation): String = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): String = parsed match {
    case SituationPlus(Situation(board, color), _) => >>(Game(board, color, turns = parsed.turns))
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

  def exportStandardPositionTurnCastling(board: Board, ply: Int): String = List(
    exportBoard(board),
    Color(ply % 2 == 0).letter,
    exportCastles(board)
  ) mkString " "

  private def exportCheckCount(board: Board) = board.history.checkCount match {
    case CheckCount(white, black) => s"+$black+$white"
  }

  private def exportCrazyPocket(board: Board) = board.crazyData match {
    case Some(variant.Crazyhouse.Data(pockets, _)) => "/" +
      pockets.white.roles.map(_.forsythUpper).mkString +
      pockets.black.roles.map(_.forsyth).mkString
    case _ => ""
  }

  private implicit val posOrdering = Ordering.by[Pos, Int](_.x)

  private[chess] def exportCastles(board: Board): String = {

    val wr = board.pieces.collect {
      case (pos, piece) if pos.y == White.backrankY && piece == White.rook => pos
    }
    val br = board.pieces.collect {
      case (pos, piece) if pos.y == Black.backrankY && piece == Black.rook => pos
    }

    val wur = board.unmovedRooks.filter(_.y == White.backrankY)
    val bur = board.unmovedRooks.filter(_.y == Black.backrankY)

    {
      // castling rights with inner rooks are represented by their file name
      (if (board.castles.whiteKingSide) (if (wur.nonEmpty && wr.max == wur.max) "K" else wur.max.file.toUpperCase) else "") +
        (if (board.castles.whiteQueenSide) (if (wur.nonEmpty && wr.min == wur.min) "Q" else wur.min.file.toUpperCase) else "") +
        (if (board.castles.blackKingSide) (if (bur.nonEmpty && br.max == bur.max) "k" else bur.max.file) else "") +
        (if (board.castles.blackQueenSide) (if (bur.nonEmpty && br.min == bur.min) "q" else bur.min.file) else "")
    } match {
      case "" => "-"
      case n  => n
    }
  }

  private[chess] def tore(pos: Pos, n: Int): Option[Pos] =
    if (n == 0) Some(pos)
    else if (n > 0) Pos.posAt(
      ((pos.x + n - 1) % 8 + 1),
      (pos.y - (pos.x + n - 1) / 8))
    else Pos.posAt(
      if (pos.x == 1) 8 else pos.x - 1,
      if (pos.x == 1) pos.y + 1 else pos.y)

  def exportBoard(board: Board): String = {
    val fen = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y ← 8 to 1 by -1) {
      empty = 0
      for (x <- 1 to 8) {
        board(x, y) match {
          case None => empty = empty + 1
          case Some(piece) =>
            if (empty == 0) fen append piece.forsyth.toString
            else {
              fen append (empty.toString + piece.forsyth)
              empty = 0
            }
            if (piece.role != Pawn && board.crazyData.fold(false)(_.promoted.exists {
              p => p.x == x && p.y == y
            })) fen append '~'
        }
      }
      if (empty > 0) fen append empty
      if (y > 1) fen append '/'
    }
    fen.toString
  }

  def getFullMove(rawSource: String): Option[Int] = read(rawSource) { fen =>
    fen.split(' ').lift(5) flatMap parseIntOption
  }

  def getColor(rawSource: String): Option[Color] = read(rawSource) { fen =>
    fen.split(' ').lift(1) flatMap (_.headOption) flatMap Color.apply
  }

  def getPly(rawSource: String): Option[Int] = read(rawSource) { fen =>
    getFullMove(fen) map { fullMove =>
      fullMove * 2 - (if (getColor(fen).exists(_.white)) 2 else 1)
    }
  }

  private def read[A](source: String)(f: String => A): A = f(source.replace("_", " ").trim)
}
