package chess
package opening

import cats.syntax.option.*
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
        case _                                                     => op.some
    }

  def isShortest(op: Opening) = shortestLines.get(op.key).contains(op)

  def findByFullFen(fen: FullFen): Option[Opening] = findByStandardFen(fen.opening)

  def findByStandardFen(fen: StandardFen): Option[Opening] = byFen.get(fen)

  val SEARCH_MAX_PLIES  = 40
  val SEARCH_MIN_PIECES = 20

  // assumes standard initial Fen and variant
  def search(sans: Iterable[SanStr]): Option[Opening.AtPly] =
    chess.Replay
      .situations(
        sans.take(SEARCH_MAX_PLIES).takeWhile(!_.value.contains('@')),
        None,
        variant.Standard
      )
      .toOption
      .flatMap(searchInSituations)

  def search(replay: Replay): Option[Opening.AtPly] =
    searchInSituations:
      val moves: Vector[Move] = replay.chronoMoves.view
        .take(SEARCH_MAX_PLIES)
        .takeWhile:
          case move: Move => move.situationBefore.board.nbPieces >= SEARCH_MIN_PIECES
          case _          => false
        .collect { case move: Move => move }
        .toVector
      moves.map(_.situationBefore) ++ moves.lastOption.map(_.situationAfter).toVector

  // first situation is initial position
  def searchInSituations(situations: Iterable[Situation]): Option[Opening.AtPly] =
    situations
      .takeWhile(_.board.nbPieces >= SEARCH_MIN_PIECES)
      .zipWithIndex
      .drop(1)
      .foldRight(none[Opening.AtPly]):
        case ((situation, ply), None) => byFen.get(format.Fen.writeOpening(situation)).map(_.atPly(Ply(ply)))
        case (_, found)               => found

  def searchInFens(fens: Iterable[StandardFen]): Option[Opening] =
    fens.foldRight(none[Opening]):
      case (fen, None) => findByStandardFen(fen)
      case (_, found)  => found
