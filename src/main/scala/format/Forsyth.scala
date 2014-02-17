package chess
package format

import Pos.{ posAt, A8 }

/**
 * Transform a game to standard Forsyth Edwards Notation
 * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 */
object Forsyth {

  val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def <<(source: String): Option[Situation] = {

    val boardChars = """\s*([\w\d/]+)\s.*""".r.replaceAllIn(
      source.replace("/", ""),
      m => m group 1
    ).toList

    val colorOption = source split " " lift 1 flatMap (_ lift 0) flatMap Color.apply

    def makePieces(chars: List[Char], pos: Pos): Option[List[(Pos, Piece)]] = chars match {
      case Nil => Some(Nil)
      case c :: rest => c match {
        case n if (n.toInt < 58) =>
          tore(pos, n.toInt - 48) flatMap { makePieces(rest, _) }
        case n => for {
          role ← Role forsyth n.toLower
        } yield (pos, Piece(Color(n.isUpper), role)) :: {
          tore(pos, 1) flatMap { makePieces(rest, _) } getOrElse Nil
        }
      }
    }

    makePieces(boardChars, A8) flatMap { pieces =>
      val board = Board(pieces, variant = chess.Variant.default)
      val c = colorOption | Color.white
      if (board check !c) {
        if (board check c) none // both sides cannot be in check
        else Situation(board, !c).some // user in check will move first
      }
      else Situation(board, c).some
    }
  }

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - (if (situation.color.white) 2 else 1)
  }

  def <<<(source: String): Option[SituationPlus] = <<(source) map { situation =>
    val history = source split " " lift 2 map { History.make(none, _) }
    val situation2 = situation withHistory (history | History.make(none, ""))
    val fullMoveNumber = source split " " lift 5 flatMap parseIntOption
    SituationPlus(situation2, fullMoveNumber | 1)
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

  def tore(pos: Pos, n: Int): Option[Pos] = posAt(
    ((pos.x + n - 1) % 8 + 1),
    (pos.y - (pos.x + n - 1) / 8)
  )

  def exportBoard(board: Board): String = {
    {
      for (y ← 8 to 1 by -1) yield {
        (1 to 8).map(board(_, y)).foldLeft(("", 0)) {
          case ((out, empty), None)        => (out, empty + 1)
          case ((out, 0), Some(piece))     => (out + piece.forsyth.toString, 0)
          case ((out, empty), Some(piece)) => (out + empty.toString + piece.forsyth, 0)
        } match {
          case (out, 0)     => out
          case (out, empty) => out + empty
        }
      } mkString
    } mkString "/"
  } mkString
}
