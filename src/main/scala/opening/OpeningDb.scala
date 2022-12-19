package chess
package opening

import cats.syntax.option.*

import chess.format.{ EpdFen, OpeningFen }
import chess.format.pgn.SanStr

object OpeningDb:

  lazy val all: Vector[Opening] =
    openingDbPartA ++ openingDbPartB ++ openingDbPartC ++ openingDbPartD ++ openingDbPartE

  private lazy val byFen: collection.Map[OpeningFen, Opening] =
    all.view.map { o =>
      o.fen -> o
    }.toMap

  lazy val families: Set[OpeningFamily] = byFen.values.map(_.family).toSet

  // Keep only one opening per unique key: the shortest one
  lazy val shortestLines: Map[OpeningKey, Opening] = OpeningDb.all
    .foldLeft(Map.empty[OpeningKey, Opening]) { case (acc, op) =>
      acc.updatedWith(op.key) {
        case Some(prev) if prev.uci.value.size < op.uci.value.size => prev.some
        case _                                                     => op.some
      }
    }

  def isShortest(op: Opening) = shortestLines get op.key contains op

  def findByEpdFen(fen: EpdFen): Option[Opening] = findByOpeningFen(fen.opening)

  def findByOpeningFen(fen: OpeningFen): Option[Opening] = byFen get fen

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial Fen and variant
  def search(sans: Iterable[SanStr]): Option[Opening.AtPly] =
    chess.Replay
      .situations(
        sans.take(SEARCH_MAX_PLIES).takeWhile(san => !san.value.contains('@')),
        None,
        variant.Standard
      )
      .toOption
      .flatMap(searchInSituations)

  def search(replay: Replay): Option[Opening.AtPly] =
    searchInSituations {
      replay.chronoMoves.view
        .takeWhile {
          case Left(_) => true
          case _       => false
        }
        .map(_.fold(_.situationAfter, _.situationAfter))
    }

  private def searchInSituations(situations: Iterable[Situation]): Option[Opening.AtPly] =
    situations.zipWithIndex.drop(1).foldRight(none[Opening.AtPly]) {
      case ((situation, ply), None) =>
        byFen get format.Fen.writeOpening(situation) map (_ atPly ply)
      case (_, found) => found
    }

  def searchInFens(fens: Iterable[OpeningFen]): Option[Opening] =
    fens.foldRight(none[Opening]) {
      case (fen, None) => findByOpeningFen(fen)
      case (_, found)  => found
    }
