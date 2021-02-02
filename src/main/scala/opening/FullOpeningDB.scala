package chess
package opening

import cats.syntax.option._

import chess.format.FEN

object FullOpeningDB {

  private lazy val byFen: collection.Map[String, FullOpening] = {
    FullOpeningPartA.db ++ FullOpeningPartB.db ++ FullOpeningPartC.db ++ FullOpeningPartD.db ++ FullOpeningPartE.db
  }.view.map { o =>
    o.fen -> o
  }.toMap

  def findByFen(fen: FEN): Option[FullOpening] =
    fen.value.split(' ').take(4) match {
      case Array(boardPocket, turn, castle, ep) =>
        val board =
          if (boardPocket.contains('[')) boardPocket.takeWhile('[' !=)
          else if (boardPocket.count('/' ==) == 8) boardPocket.split('/').take(8).mkString("/")
          else boardPocket
        byFen get List(board, turn, castle, ep).mkString(" ")
      case _ => None
    }

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(moveStrs: Iterable[String]): Option[FullOpening.AtPly] =
    chess.Replay
      .situations(
        moveStrs.take(SEARCH_MAX_PLIES).takeWhile(san => !san.contains('@')),
        None,
        variant.Standard
      )
      .toOption
      .flatMap {
        _.zipWithIndex.drop(1).foldRight(none[FullOpening.AtPly]) {
          case ((situation, ply), None) =>
            val fen = format.Forsyth.exportStandardPositionTurnCastlingEp(situation)
            byFen get fen map (_ atPly ply)
          case (_, found) => found
        }
      }

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] =
    fens.foldRight(none[FullOpening]) {
      case (fen, None) => findByFen(fen)
      case (_, found)  => found
    }
}
