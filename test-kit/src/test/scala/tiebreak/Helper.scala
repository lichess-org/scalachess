package chess
package tiebreak

import chess.format.pgn.PgnStr
import chess.rating.Elo
import chess.tiebreak.Tiebreak.*

object Helper:

  def parsedTags(pgnLines: List[String]) =
    pgnLines.flatMap(pgnstr => chess.format.pgn.Parser.tags(PgnStr(pgnstr)).toOption)

  def playerFromTag(
      name: Option[String],
      rating: Option[IntRating],
      fideId: Option[Int]
  ): Option[Player] =
    fideId
      .map(_.toString)
      .orElse(name)
      .map: id =>
        Player(id, rating.map(_.into(Elo)))

  def tiebreakGames(pgnSplit: List[String]): List[(Player, Game)] =
    parsedTags(pgnSplit).foldLeft(List.empty): (acc, tags) =>
      val names = tags.names
      val ratings = tags.ratings
      val fideIds = tags.fideIds
      val result = tags.outcome
      val white = playerFromTag(names.white.map(_.value), ratings.white, fideIds.white.map(_.value))
      val black = playerFromTag(names.black.map(_.value), ratings.black, fideIds.black.map(_.value))
      val roundId = tags.roundNumber.map(_.toString)
      val byColorPoints = result.map(chess.Outcome.outcomeToPoints)
      (white, black, byColorPoints) match
        case (Some(w), Some(b), Some(points)) =>
          List(
            w -> Game(points.white, b, White, roundId),
            b -> Game(points.black, w, Black, roundId)
          ) ++ acc
        case _ => acc

  def games(fileName: String): Map[String, PlayerWithGames] =
    val pgnText = scala.io.Source.fromResource(fileName).mkString
    val pgnSplit = pgnText.split("\n\n").toList

    tiebreakGames(pgnSplit)
      .groupBy(_._1)
      .map: (player, games) =>
        player.id -> PlayerWithGames(player, games.map(_._2))
