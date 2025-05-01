package chess
package format

import cats.syntax.all.*
import chess.bitboard.Bitboard

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenWriter:

  private given Ordering[File] = Ordering.by[File, Int](_.value)
  given Ordering[Square]       = Ordering.by[Square, File](_.file)

  def write(board: Position): FullFen =
    write(board, FullMoveNumber(1))

  def write(parsed: Position.AndFullMoveNumber): FullFen =
    write(parsed.board, parsed.fullMoveNumber)

  def write(game: Game): FullFen =
    write(game.board, game.ply.fullMoveNumber)

  def write(board: Position, fullMoveNumber: FullMoveNumber): FullFen = FullFen:
    val builder = scala.collection.mutable.StringBuilder(80)
    builder.append(writeBoard(board))
    builder.append(writeCrazyPocket(board))
    builder.addOne(' ')
    builder.addOne(board.color.letter)
    builder.addOne(' ')
    builder.append(writeCastles(board))
    builder.addOne(' ')
    builder.append(board.enPassantSquare.fold("-")(_.key))
    builder.addOne(' ')
    builder.append(board.history.halfMoveClock)
    builder.addOne(' ')
    builder.append(fullMoveNumber)
    if board.variant == variant.ThreeCheck then
      builder.addOne(' ')
      builder.append(writeCheckCount(board))
    builder.toString

  def writeOpening(board: Position): StandardFen = StandardFen:
    s"${writeBoard(board)} ${board.color.letter} ${writeCastles(board)} ${board.enPassantSquare
        .fold("-")(_.key)}"

  def writeBoard(board: Position): BoardFen =
    val fen   = scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for y <- Rank.allReversed do
      empty = 0
      for x <- File.all do
        board(x, y) match
          case None => empty = empty + 1
          case Some(piece) =>
            if empty == 0 then fen.append(piece.forsyth.toString)
            else
              fen.append(s"$empty${piece.forsyth}")
              empty = 0
            if piece.role != Pawn && board.crazyData.exists(_.promoted.contains(Square(x, y))) then
              fen.append('~')
      if empty > 0 then fen.append(empty)
      if y > Rank.First then fen.append('/')
    BoardFen(fen.toString)

  def writeBoardAndColor(board: Position): BoardAndColorFen =
    writeBoardAndColor(board, board.color)

  def writeBoardAndColor(board: Position, turnColor: Color): BoardAndColorFen =
    writeBoard(board).andColor(turnColor)

  private def writeCheckCount(board: Position) =
    board.history.checkCount match
      case CheckCount(white, black) => s"+$black+$white"

  private def writeCrazyPocket(board: Position) =
    board.crazyData match
      case Some(variant.Crazyhouse.Data(pockets, _)) =>
        "/" + pockets.forsyth
      case _ => ""

  private[chess] def writeCastles(board: Position): String =
    val wr  = board.rooks & board.white & Bitboard.rank(White.backRank)
    val br  = board.rooks & board.black & Bitboard.rank(Black.backRank)
    val wur = board.unmovedRooks.without(Black).bb
    val bur = board.unmovedRooks.without(White).bb
    (if board.castles.whiteKingSide then
       wur.last
         .map(sq => if wr.last.contains(sq) then "K" else sq.file.toUpperCaseString)
         .getOrElse("K")
     else "") +
      (if board.castles.whiteQueenSide then
         wur.first
           .map(sq => if wr.first.contains(sq) then "Q" else sq.file.toUpperCaseString)
           .getOrElse("Q")
       else "") +
      (if board.castles.blackKingSide then
         bur.last
           .map(sq => if br.last.contains(sq) then "k" else sq.file.char.toString)
           .getOrElse("k")
       else "") +
      (if board.castles.blackQueenSide then
         bur.first
           .map(sq => if br.first.contains(sq) then "q" else sq.file.char.toString)
           .getOrElse("q")
       else "") match
      case "" => "-"
      case s  => s
