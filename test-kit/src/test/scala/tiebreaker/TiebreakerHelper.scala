package chess
package tiebreaker

import chess.format.pgn.PgnStr
import chess.rating.Elo
import chess.tiebreaker.Tiebreaker.*

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

  def tiebreakerGames(pgnSplit: List[String]): List[(Player, Game)] =
    parsedTags(pgnSplit).foldLeft(List.empty): (acc, tags) =>
      val names         = tags.names
      val ratings       = tags.ratings
      val fideIds       = tags.fideIds
      val result        = tags.outcome
      val white         = playerFromTag(names.white.map(_.value), ratings.white, fideIds.white.map(_.value))
      val black         = playerFromTag(names.black.map(_.value), ratings.black, fideIds.black.map(_.value))
      val byColorPoints = result.map(chess.Outcome.outcomeToPoints)
      (white, black) match
        case (Some(w), Some(b)) =>
          List(
            w -> Game(byColorPoints.map(_.white), b, White),
            b -> Game(byColorPoints.map(_.black), w, Black)
          ) ++ acc
        case _ => acc

  def games(fileName: String): Map[String, PlayerWithGames] =
    val pgnText  = scala.io.Source.fromResource(fileName).mkString
    val pgnSplit = pgnText.split("\n\n").toList

    tiebreakerGames(pgnSplit)
      .groupBy(_._1)
      .map: (player, games) =>
        player.id -> PlayerWithGames(player, games.map(_._2))
