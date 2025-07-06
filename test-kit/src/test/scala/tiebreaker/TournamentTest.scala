package chess
package tiebreaker

import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions
import chess.tiebreaker.Tiebreaker.*
import chess.format.pgn.PgnStr
import chess.rating.Elo

import chess.Outcome.Points

class TournamentTest extends MunitExtensions with SnapshotAssertions:

  val pgnText = scala.io.Source.fromResource("FWWRC.pgn").mkString

  val pgnSplit = pgnText.split("\n\n").toList

  val parsedTags = pgnSplit.flatMap(pgnstr => chess.format.pgn.Parser.tags(PgnStr(pgnstr)).toOption)

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

  case class Game(white: Player, black: Player, result: Option[ByColor[Points]]):
    def toPovGame: ByColor[POVGame] =
      ByColor(
        white = POVGame(result.map(_(Color.White)), black, Color.White),
        black = POVGame(result.map(_(Color.Black)), white, Color.Black)
      )

  val tiebreakerGames: Seq[Game] = parsedTags.foldLeft(Seq.empty[Game]): (acc, tags) =>
    val names         = tags.names
    val ratings       = tags.ratings
    val fideIds       = tags.fideIds
    val result        = tags.outcome
    val white         = playerFromTag(names.white.map(_.value), ratings.white, fideIds.white.map(_.value))
    val black         = playerFromTag(names.black.map(_.value), ratings.black, fideIds.black.map(_.value))
    val byColorPoints = result.map(chess.Outcome.outcomeToPoints)
    (white, black) match
      case (Some(w), Some(b)) =>
        Game(w, b, byColorPoints) +: acc
      case _ => acc

  // Flatten all POVGames from tiebreakerGames, associating each with its player
  val povGamesWithPlayer: Seq[(Player, POVGame)] = tiebreakerGames.flatMap: g =>
    Seq(
      g.white -> g.toPovGame.white,
      g.black -> g.toPovGame.black
    )

  val allGames = povGamesWithPlayer
    .groupBy(_._1)
    .map: (player, games) =>
      player.uniqueIdentifier -> PlayerGames(player, games.map(_._2))

  test("tiebreaker games snapshot"):
    val x = Tiebreaker.compute(
      allGames,
      List(
        AverageOfOpponentsBuchholz,
        AveragePerfectPerformanceOfOpponents,
        DirectEncounter,
        PerfectTournamentPerformance,
        SonnebornBerger
      )
      ).mkString("\n")
    assertFileSnapshot(x, "tiebreaker/tournament.txt")
