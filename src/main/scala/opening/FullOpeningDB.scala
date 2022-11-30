package chess
package opening

import cats.syntax.option.*

import chess.format.{ Fen, OpeningFen }

object FullOpeningDB:

  lazy val all: Vector[FullOpening] =
    FullOpeningPartA.db ++ FullOpeningPartB.db ++ FullOpeningPartC.db ++ FullOpeningPartD.db ++ FullOpeningPartE.db

  private lazy val byFen: collection.Map[OpeningFen, FullOpening] =
    all.view.map { o =>
      o.fen -> o
    }.toMap

  lazy val families: Set[OpeningFamily] = byFen.values.map(_.family).toSet

  // Keep only one opening per unique key: the shortest one
  lazy val shortestLines: Map[OpeningKey, FullOpening] = FullOpeningDB.all
    .foldLeft(Map.empty[OpeningKey, FullOpening]) { case (acc, op) =>
      acc.updatedWith(op.key) {
        case Some(prev) if prev.uci.value.size < op.uci.value.size => prev.some
        case _                                                     => op.some
      }
    }

  def isShortest(op: FullOpening) = shortestLines get op.key contains op

  def findByFullFen(fen: Fen): Option[FullOpening] = findByFen(OpeningFen fromFen fen)

  def findByFen(fen: OpeningFen): Option[FullOpening] = byFen get fen

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial Fen and variant
  def search(moveStrs: Iterable[String]): Option[FullOpening.AtPly] =
    chess.Replay
      .situations(
        moveStrs.take(SEARCH_MAX_PLIES).takeWhile(san => !san.contains('@')),
        None,
        variant.Standard
      )
      .toOption
      .flatMap(searchInSituations)

  def search(replay: Replay): Option[FullOpening.AtPly] =
    searchInSituations {
      replay.chronoMoves.view
        .takeWhile {
          case Left(_) => true
          case _       => false
        }
        .map(_.fold(_.situationAfter, _.situationAfter))
    }

  private def searchInSituations(situations: Iterable[Situation]): Option[FullOpening.AtPly] =
    situations.zipWithIndex.drop(1).foldRight(none[FullOpening.AtPly]) {
      case ((situation, ply), None) =>
        byFen get format.Forsyth.openingFen(situation) map (_ atPly ply)
      case (_, found) => found
    }

  def searchInFens(fens: Iterable[OpeningFen]): Option[FullOpening] =
    fens.foldRight(none[FullOpening]) {
      case (fen, None) => findByFen(fen)
      case (_, found)  => found
    }
