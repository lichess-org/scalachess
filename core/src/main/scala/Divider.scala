package chess

import cats.syntax.all.*

import scala.annotation.switch

case class Division(middle: Option[Ply], end: Option[Ply], plies: Ply):

  def openingSize: Ply                  = middle | plies
  def middleSize: Option[Ply]           = middle.map((end | plies) - _)
  def endSize: Option[Ply]              = end.map(plies - _)
  def openingBounds: Option[(Int, Ply)] = middle.map(0 -> _)
  def middleBounds: Option[(Ply, Ply)]  = (middle, end).tupled
  def endBounds: Option[(Ply, Ply)]     = end.map(_ -> plies)

object Division:
  val empty = Division(None, None, Ply.initial)

object Divider:

  def apply(positions: List[Position]): Division =

    val indexedPositions: List[(Position, Int)] = positions.zipWithIndex

    val midGame = indexedPositions.collectFirst:
      case (position, index)
          if (majorsAndMinors(position) <= 10 || backrankSparse(position) || mixedness(position) > 150) =>
        index

    val endGame =
      if midGame.isDefined then
        indexedPositions.collectFirst:
          case (position, index) if majorsAndMinors(position) <= 6 => index
      else None

    Division(
      Ply.from(midGame.filter(m => endGame.fold(true)(m < _))),
      Ply.from(endGame),
      Ply(positions.size)
    )

  private def majorsAndMinors(position: Position): Int =
    (position.occupied & ~(position.kings | position.pawns)).count

  // Sparse back-rank indicates that pieces have been developed
  private def backrankSparse(position: Position): Boolean =
    (Bitboard.firstRank & position.white).count < 4 ||
      (Bitboard.lastRank & position.black).count < 4

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

  private def mixedness(position: Position): Int =
    mixednessRegions.foldLeft(0):
      case (acc, (region, y)) =>
        acc + position.byColor.mapReduce(c => (c & region).count)(score(y))
