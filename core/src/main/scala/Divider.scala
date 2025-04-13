package chess

import cats.syntax.all.*

import scala.annotation.switch

import bitboard.Bitboard

case class Division(middle: Option[Ply], end: Option[Ply], plies: Ply):

  def openingSize: Ply        = middle | plies
  def middleSize: Option[Ply] = middle.map((end | plies) - _)
  def endSize                 = end.map(plies - _)
  def openingBounds           = middle.map(0 -> _)
  def middleBounds            = (middle, end).tupled
  def endBounds               = end.map(_ -> plies)

object Division:
  val empty = Division(None, None, Ply.initial)

object Divider:

  def apply(boards: List[Board]): Division =

    val indexedBoards: List[(Board, Int)] = boards.zipWithIndex

    val midGame = indexedBoards.collectFirst:
      case (board, index)
          if (majorsAndMinors(board) <= 10 ||
            backrankSparse(board) ||
            mixedness(board) > 150) =>
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

  private def score(y: Int)(white: Int, black: Int): Int =
    ((white, black): @switch) match
      case (0, 0) => 0

      case (1, 0) => 1 + (8 - y)
      case (2, 0) => if y > 2 then 2 + (y - 2) else 0
      case (3, 0) => if y > 1 then 3 + (y - 1) else 0
      case (4, 0) =>
        if y > 1 then 3 + (y - 1) else 0 // group of 4 on the homerow = 0

      case (0, 1) => 1 + y
      case (1, 1) => 5 + (3 - y).abs
      case (2, 1) => 4 + y
      case (3, 1) => 5 + y

      case (0, 2) => if y < 6 then 2 + (6 - y) else 0
      case (1, 2) => 4 + (6 - y)
      case (2, 2) => 7

      case (0, 3) => if y < 7 then 3 + (7 - y) else 0
      case (1, 3) => 5 + (6 - y)

      case (0, 4) => if y < 7 then 3 + (7 - y) else 0

      case _ => 0

  private val mixednessRegions: List[(Long, Int)] = {
    val smallSquare = 0x0303L
    for
      y <- 0 to 6
      x <- 0 to 6
    yield (smallSquare << (x + 8 * y), y + 1)
  }.toList

  private def mixedness(board: Board): Int =
    mixednessRegions.foldLeft(0):
      case (acc, (region, y)) =>
        acc + board.byColor.mapReduce(c => (c & region).count)(score(y))
