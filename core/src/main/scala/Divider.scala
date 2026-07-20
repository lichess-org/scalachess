package chess

import cats.syntax.all.*

import scala.annotation.switch

case class Division(middle: Option[Ply], end: Option[Ply], plies: Ply):

  def openingSize: Ply = middle | plies
  def middleSize: Option[Ply] = middle.map((end | plies) - _)
  def endSize: Option[Ply] = end.map(plies - _)
  def openingBounds: Option[(Int, Ply)] = middle.map(0 -> _)
  def middleBounds: Option[(Ply, Ply)] = (middle, end).tupled
  def endBounds: Option[(Ply, Ply)] = end.map(_ -> plies)

object Division:
  val empty = Division(None, None, Ply.initial)

object Divider:

  def apply(boards: List[Board]): Division =

    val indexedBoards: List[(Board, Int)] = boards.zipWithIndex

    val midGame = indexedBoards.collectFirst:
      case (board, index)
          if (majorsAndMinors(board) <= 10 || backrankSparse(board) || mixedness(board) > 150) =>
        index

    val endGame =
      if midGame.isDefined then
        indexedBoards.collectFirst:
          case (board, index) if majorsAndMinors(board) <= 6 => index
      else None

    Division(
      Ply.from(midGame.filter(m => endGame.fold(true)(m < _))),
      Ply.from(endGame),
      Ply(boards.size)
    )

  private def majorsAndMinors(board: Board): Int =
    (board.occupied & ~(board.kings | board.pawns)).count

  // Sparse back-rank indicates that pieces have been developed
  private def backrankSparse(board: Board): Boolean =
    (Bitboard.firstRank & board.white).count < 4 ||
      (Bitboard.lastRank & board.black).count < 4

  private def score(y: Int, white: Int, black: Int): Int =
    (white: @switch) match
      case 0 =>
        (black: @switch) match
          case 1 => 1 + y
          case 2 => if y < 6 then 2 + (6 - y) else 0
          case 3 => if y < 7 then 3 + (7 - y) else 0
          case 4 => if y < 7 then 3 + (7 - y) else 0
          case _ => 0
      case 1 =>
        (black: @switch) match
          case 0 => 1 + (8 - y)
          case 1 => 5 + (4 - y).abs
          case 2 => 4 + (7 - y)
          case 3 => 5 + (7 - y)
          case _ => 0
      case 2 =>
        (black: @switch) match
          case 0 => if y > 2 then 2 + (y - 2) else 0
          case 1 => 4 + (y - 1)
          case 2 => 7
          case _ => 0
      case 3 =>
        (black: @switch) match
          case 0 => if y > 1 then 3 + (y - 1) else 0
          case 1 => 5 + (y - 1)
          case _ => 0
      case 4 =>
        (black: @switch) match
          case 0 => if y > 1 then 3 + (y - 1) else 0 // group of 4 on the homerow = 0
          case _ => 0
      case _ => 0

  private val mixednessRegions: Array[Long] = {
    val smallSquare = 0x0303L
    for
      y <- 0 to 6
      x <- 0 to 6
    yield smallSquare << (x + 8 * y)
  }.toArray

  private def mixedness(board: Board): Int =
    var acc = 0
    var i = 0
    while i < mixednessRegions.length do
      val region = mixednessRegions(i)
      val y = i / 7 + 1
      acc += score(y, (board.white & region).count, (board.black & region).count)
      i += 1
    acc
