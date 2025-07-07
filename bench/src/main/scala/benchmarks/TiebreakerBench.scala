package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.Color
import chess.Outcome.Points
import chess.rating.Elo
import chess.tiebreaker.Tiebreaker.*
import chess.tiebreaker.TieBreakPoints
import chess.format.pgn.PgnStr
import chess.IntRating
import chess.ByColor
import chess.tiebreaker.Tiebreaker
import chess.tiebreaker.*
import chess.File.B

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class TiebreakerBench:

  private val Work: Long = 10

  var allGames: Map[PlayerId, PlayerGames] = scala.compiletime.uninitialized
  var tournament: Tournament               = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =

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

    allGames = povGamesWithPlayer
      .groupBy(_._1)
      .map: (player, games) =>
        player.uniqueIdentifier -> PlayerGames(player, games.map(_._2))
      .toMap
    tournament = Tournament(allGames)

  @Benchmark
  def averageOfOpponentsBuchholz(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AverageOfOpponentsBuchholz.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averagePerfectPerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AveragePerfectPerformanceOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averagePerfectPerformanceOfOpponentsAll(bh: Blackhole) =
    bh.consume:
      AveragePerfectPerformanceOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def directEncounter(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        DirectEncounter.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def directEncounterAll(bh: Blackhole) =
    bh.consume:
      DirectEncounter.compute(tournament, Map.empty)

  @Benchmark
  def perfectTournamentPerformance(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        PerfectTournamentPerformance.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def perfectTournamentPerformanceAll(bh: Blackhole) =
    bh.consume:
      PerfectTournamentPerformance.compute(tournament, Map.empty)

  @Benchmark
  def sonnebornBerger(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        SonnebornBerger.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def tournamentPerformance(bh: Blackhole) =
    bh.consume:
      Tiebreaker.compute(
        allGames,
        List(
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          DirectEncounter,
          PerfectTournamentPerformance,
          SonnebornBerger
        )
      )

  @Benchmark
  def averageRatingOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AverageRatingOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averageRatingOfOpponentsAll(bh: Blackhole) =
    bh.consume:
      AverageRatingOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def foreBuchholz(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        ForeBuchholz.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def koyaSystem(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        KoyaSystem.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def blackPlayedGames(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbBlackGames.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def blackWonGames(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbBlackWins.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def gamesWon(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbWins.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def tournamentPerformanceRating(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        TournamentPerformanceRating.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averagePerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AveragePerformanceOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def progressiveScores(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        SumOfProgressiveScores.compute(pg.player, allGames, Map.empty)
