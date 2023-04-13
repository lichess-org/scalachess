package chess
package format

import cats.syntax.all.*
import ornicar.scalalib.zeros.given

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenWriter:

  private given Ordering[File] = intOrdering[File]
  given Ordering[Square]          = Ordering.by[Square, File](_.file)

  def write(situation: Situation): EpdFen = write(Situation.AndFullMoveNumber(situation, FullMoveNumber(1)))

  def write(parsed: Situation.AndFullMoveNumber): EpdFen =
    write(Game(parsed.situation, ply = parsed.ply))

  def write(game: Game): EpdFen = EpdFen:
    {
      List[String](
        s"${writeBoard(game.board)}${writeCrazyPocket(game.board)}",
        game.player.letter.toString,
        writeCastles(game.board),
        game.situation.enPassantSquare.fold("-")(_.key),
        game.halfMoveClock.toString,
        game.fullMoveNumber.toString
      ) ::: (game.board.variant == variant.ThreeCheck) ?? List(writeCheckCount(game.board))
    } mkString " "

  def writeOpening(situation: Situation): OpeningFen = OpeningFen:
    s"${writeBoard(situation.board)} ${situation.color.letter} ${writeCastles(situation.board)} ${situation.enPassantSquare
        .fold("-")(_.key)}"

  def writeBoard(board: Board): BoardFen =
    val fen   = scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.allReversed)
      empty = 0
      for (x <- File.all)
        board(x, y) match
          case None => empty = empty + 1
          case Some(piece) =>
            if (empty == 0) fen append piece.forsyth.toString
            else
              fen append s"$empty${piece.forsyth}"
              empty = 0
            if (piece.role != Pawn && board.crazyData.exists(_.promoted.contains(Square(x, y))))
              fen append '~'
      if (empty > 0) fen append empty
      if (y > Rank.First) fen append '/'
    BoardFen(fen.toString)

  def writeBoardAndColor(situation: Situation): BoardAndColorFen =
    writeBoardAndColor(situation.board, situation.color)

  def writeBoardAndColor(board: Board, turnColor: Color): BoardAndColorFen =
    writeBoard(board).andColor(turnColor)

  private def writeCheckCount(board: Board) =
    board.history.checkCount match
      case CheckCount(white, black) => s"+$black+$white"

  private def writeCrazyPocket(board: Board) =
    board.crazyData match
      case Some(variant.Crazyhouse.Data(pockets, _)) =>
        "/" + pockets.forsyth
      case _ => ""

  private[chess] def writeCastles(board: Board): String =
    board.castles.toFenString
