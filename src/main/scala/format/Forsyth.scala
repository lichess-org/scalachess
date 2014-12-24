package chess
package format

/**
 * Transform a game to standard Forsyth Edwards Notation
 * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 */
object Forsyth {

  val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def <<(source: String): Option[Situation] = {

    val boardChars = source.trim.takeWhile(' '!=).replace("/", "").toList

    val colorOption = source split " " lift 1 flatMap (_ lift 0) flatMap Color.apply

    makeBoard(source) flatMap { board =>
      val c = colorOption | White
      if (board check !c) {
        if (board check c) none // both sides cannot be in check
        else Situation(board, !c).some // user in check will move first
      }
      else Situation(board, c).some
    }
  }

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(source: String): Option[Board] =
    makePieces(source.trim.takeWhile(' '!=).replace("/", "").toList, Pos.A8) map { pieces =>
      Board(pieces, variant = chess.Variant.default)
    }

  private def makePieces(chars: List[Char], pos: Pos): Option[List[(Pos, Piece)]] = chars match {
    case Nil => Some(Nil)
    case c :: rest => c match {
      case n if n.toInt < 58 =>
        makePieces(rest, tore(pos, n.toInt - 48) getOrElse pos)
      case n => Role forsyth n.toLower map { role =>
        (pos, Piece(Color(n.isUpper), role)) :: {
          tore(pos, 1) flatMap { makePieces(rest, _) } getOrElse Nil
        }
      }
    }
  }

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - (if (situation.color.white) 2 else 1)
  }

  def <<<(source: String): Option[SituationPlus] = {
    val fixedSource = fixCastles(source) | source
    <<(fixedSource) map { situation =>
      val history = fixedSource split " " lift 2 map { History.make(none, _) }
      val situation2 = situation withHistory (history | History.make(none, ""))
      val fullMoveNumber = fixedSource split " " lift 5 flatMap parseIntOption
      SituationPlus(situation2, fullMoveNumber | 1)
    }
  }

  def >>(situation: Situation): String = >>(SituationPlus(situation, 0))

  def >>(parsed: SituationPlus): String = parsed match {
    case SituationPlus(Situation(board, color), _) => >>(Game(board, color, turns = parsed.turns))
  }

  def >>(game: Game): String = List(
    exportBoard(game.board),
    game.player.letter,
    game.board.history.castles.toString,
    ((for {
      lastMove ← game.board.history.lastMove
      (orig, dest) = lastMove
      piece ← game board dest
      if piece is Pawn
      pos ← if (orig.y == 2 && dest.y == 4) dest.down
      else if (orig.y == 7 && dest.y == 5) dest.up
      else None
    } yield pos.toString) getOrElse "-"),
    game.halfMoveClock,
    game.fullMoveNumber
  ) mkString " "

  def tore(pos: Pos, n: Int): Option[Pos] = Pos.posAt(
    ((pos.x + n - 1) % 8 + 1),
    (pos.y - (pos.x + n - 1) / 8)
  )

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
        }
      }
      if (empty > 0) fen append empty
      if (y > 1) fen append "/"
    }
    fen.toString
  }

  def fixCastles(fen: String): Option[String] = fen.trim.split(' ').toList match {
    case boardStr :: color :: castlesStr :: rest => makeBoard(boardStr) map { board =>
      val c1 = Castles(castlesStr)
      val wkPos = board.kingPosOf(White)
      val bkPos = board.kingPosOf(Black)
      val wkReady = wkPos.fold(false)(_.y == 1)
      val bkReady = bkPos.fold(false)(_.y == 8)
      def rookReady(color: Color, kPos: Option[Pos], left: Boolean) = kPos.fold(false) { kp =>
        board actorsOf color exists { a =>
          a.piece.role == Rook && a.pos.y == kp.y && (left ^ (a.pos.x > kp.x))
        }
      }
      val c2 = Castles(
        whiteKingSide = c1.whiteKingSide && wkReady && rookReady(White, wkPos, false),
        whiteQueenSide = c1.whiteQueenSide && wkReady && rookReady(White, wkPos, true),
        blackKingSide = c1.blackKingSide && bkReady && rookReady(Black, bkPos, false),
        blackQueenSide = c1.blackQueenSide && bkReady && rookReady(Black, bkPos, true))
      s"$boardStr $color $c2 ${rest.mkString(" ")}"
    }
    case _ => None
  }
}
