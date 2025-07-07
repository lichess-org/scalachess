package chess
package tiebreaker

import chess.Outcome.Points
import chess.format.pgn.PgnStr
import chess.rating.Elo
import chess.tiebreaker.Tiebreaker.*
import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

class TournamentTest extends MunitExtensions with SnapshotAssertions:

  val pgnText = scala.io.Source.fromResource("FWWRC.pgn").mkString

  val pgnSplit = pgnText.split("\n\n").toList

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

  case class Game(white: Player, black: Player, result: Option[ByColor[Points]]):
    def toPovGame: ByColor[POVGame] =
      ByColor(
        white = POVGame(result.map(_(Color.White)), black, Color.White),
        black = POVGame(result.map(_(Color.Black)), white, Color.Black)
      )

  def tiebreakerGames(pgnSplit: List[String]): Seq[Game] = parsedTags(pgnSplit).foldLeft(Seq.empty[Game]):
    (acc, tags) =>
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
  def povGamesWithPlayer(pgnSplit: List[String]): Seq[(Player, POVGame)] = tiebreakerGames(pgnSplit).flatMap:
    g =>
      Seq(
        g.white -> g.toPovGame.white,
        g.black -> g.toPovGame.black
      )

  def games(pgnSplit: List[String]) = povGamesWithPlayer(pgnSplit)
    .groupBy(_._1)
    .map: (player, games) =>
      player.uniqueIdentifier -> PlayerGames(player, games.map(_._2))

  test("tiebreaker games snapshot") {
    val result = Tiebreaker
      .compute(
        games(pgnSplit),
        List(
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          DirectEncounter,
          PerfectTournamentPerformance,
          SonnebornBerger
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/tournament.txt")
  }

  // https://chess-results.com/tnr1074691.aspx?lan=1&art=1&flag=30
  test("Woment world rapid championship") {
    val result = Tiebreaker
      .compute(
        games(pgnSplit),
        List(
          BuchholzCut1,
          Buchholz,
          AverageRatingOfOpponentsCut1
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/official_tournament.txt")
  }

  // https://chess-results.com/tnr1175851.aspx?art=1
  test("Uzchess Cup"):
    val pgnText  = scala.io.Source.fromResource("uzchesscup.pgn").mkString
    val pgnSplit = pgnText.split("\n\n").toList
    val result   = Tiebreaker
      .compute(
        games(pgnSplit),
        List(
          DirectEncounter,
          SonnebornBerger,
          NbWins,
          NbBlackWins,
          KoyaSystem
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/uzchesscup.txt")
