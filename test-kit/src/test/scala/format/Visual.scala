package chess
package format

import chess.variant.{ Crazyhouse, Variant }

/** r bqkb r
  * p ppp pp
  * pr
  *    P p
  *    QnB
  *  PP  N
  * P    PPP
  * RN  K  R
  */
object Visual:

  def <<(source: String): Position =
    val lines    = augmentString(source).linesIterator.to(List)
    val filtered = lines.size match
      case 8          => lines
      case n if n > 8 => lines.slice(1, 9)
      case n          => (List.fill(8 - n)("")) ::: lines
    val p = createPosition(
      pieces = (for
        (l, y) <- filtered.zipWithIndex
        (c, x) <- l.zipWithIndex
        role   <- Role.forsyth(c.toLower)
      yield Square.at(x, 7 - y).map { square =>
        square -> (Color.fromWhite(c.isUpper) - role)
      }).flatten,
      variant = chess.variant.Variant.default
    )
    p.updateHistory(_ => History(unmovedRooks = UnmovedRooks.from(p.board), crazyData = None))

  def >>(board: Position): String = >>|(board, Map.empty)

  def >>|(board: Position, marks: Map[Iterable[Square], Char]): String = {
    val markedPoss: Map[Square, Char] = marks.foldLeft(Map[Square, Char]()) { case (marks, (poss, char)) =>
      marks ++ (poss.toList.map { square =>
        (square, char)
      })
    }
    for (y <- Rank.allReversed) yield {
      for (x <- File.all) yield
        val square = Square(x, y)
        markedPoss.get(square).getOrElse(board.pieceAt(square).fold(' ')(_.forsyth))
    }.mkString
  }.map { """\s*$""".r.replaceFirstIn(_, "") }.mkString("\n")

  def addNewLines(str: String) = "\n" + str + "\n"

  def createPosition(pieces: Iterable[(Square, Piece)], variant: Variant): Position =
    val board        = Board.fromMap(pieces.toMap)
    val unmovedRooks = if variant.allowsCastling then UnmovedRooks(board.rooks) else UnmovedRooks.none
    Position(
      board,
      History(
        castles = variant.castles,
        unmovedRooks = unmovedRooks,
        crazyData = variant.crazyhouse.option(Crazyhouse.Data.init)
      ),
      variant,
      White
    )
