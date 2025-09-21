package chess
package opening

import cats.Foldable
import cats.syntax.all.*
import chess.format.pgn.SanStr
import chess.format.{ FullFen, StandardFen }

object OpeningDb:

  lazy val all: Vector[Opening] =
    openingDbPartA ++ openingDbPartB ++ openingDbPartC ++ openingDbPartD ++ openingDbPartE

  private lazy val byFen: collection.Map[StandardFen, Opening] = all.mapBy(_.fen)

  lazy val families: Set[OpeningFamily] = byFen.values.map(_.family).toSet

  // Keep only one opening per unique key: the shortest one
  lazy val shortestLines: Map[OpeningKey, Opening] = OpeningDb.all
    .foldLeft(Map.empty) { case (acc, op) =>
      acc.updatedWith(op.key):
        case Some(prev) if prev.uci.value.size < op.uci.value.size => prev.some
        case _ => op.some
    }

  def isShortest(op: Opening) = shortestLines.get(op.key).contains(op)

  def findByFullFen(fen: FullFen): Option[Opening] = findByStandardFen(fen.opening)

  def findByStandardFen(fen: StandardFen): Option[Opening] = byFen.get(fen)

  val SEARCH_MAX_PLIES = 40
  val SEARCH_MIN_PIECES = 20

  // assumes standard initial Fen and variant
  def search(sans: Iterable[SanStr]): Option[Opening.AtPly] =
    chess.variant.Standard.initialPosition
      .playPositions(sans.take(SEARCH_MAX_PLIES).takeWhile(!_.value.contains('@')).toList)
      .toOption
      .flatMap(searchInPositions)

  @scala.annotation.targetName("searchMoveOrDrops")
  def search(moveOrDrops: Iterable[MoveOrDrop]): Option[Opening.AtPly] =
    searchInPositions:
      val moves: Vector[Move] = moveOrDrops.view
        .take(SEARCH_MAX_PLIES)
        .takeWhile:
          case move: Move => move.before.board.nbPieces >= SEARCH_MIN_PIECES
          case _ => false
        .collect { case move: Move => move }
        .toVector
      moves.map(_.before) ++ moves.lastOption.map(_.after).toVector

  // first position is initial position
  def searchInPositions[F[_]: Foldable](positions: F[Position]) =
    positions
      .takeWhile_(_.board.nbPieces >= SEARCH_MIN_PIECES)
      .zipWithIndex
      .drop(1)
      .foldRight(none[Opening.AtPly]):
        case ((board, ply), None) => byFen.get(format.Fen.writeOpening(board)).map(_.atPly(Ply(ply)))
        case (_, found) => found

  def searchInFens(fens: Iterable[StandardFen]): Option[Opening] =
    fens.foldRight(none[Opening]):
      case (fen, None) => findByStandardFen(fen)
      case (_, found) => found
