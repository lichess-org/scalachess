package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

import cats.syntax.all.*
import chess.Color
import chess.Outcome.Points
import chess.rating.Elo
import chess.tiebreaker.Tiebreaker.*
import chess.tiebreaker.TieBreakPoints
import chess.format.pgn.PgnStr
import chess.IntRating
import chess.ByColor
import os.FileType.Dir
import chess.tiebreaker.Tiebreaker
import chess.File.A

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class TiebreakerBench:

  private val Work: Long = 10

  var allGames: Seq[PlayerGames] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =

    val pgnText = scala.io.Source.fromResource("FWWRC.pgn").mkString

    val pgnSplit = pgnText.split("\n\n\n").toList

    val parsedGames = pgnSplit.map(pgnstr => chess.format.pgn.Parser.full(PgnStr(pgnstr)).toOption).flatten

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

    val tiebreakerGames: Seq[Game] = parsedGames.foldLeft(Seq.empty[Game]): (acc, pgn) =>
      val names         = pgn.tags.names
      val ratings       = pgn.tags.ratings
      val fideIds       = pgn.tags.fideIds
      val result        = pgn.tags.outcome
      val white         = playerFromTag(names.white.map(_.value), ratings.white, fideIds.white.map(_.value))
      val black         = playerFromTag(names.black.map(_.value), ratings.black, fideIds.black.map(_.value))
      val byColorPoints = result.map(chess.Outcome.outcomeToPoints)
      (white, black) match
        case (Some(w), Some(b)) =>
          acc :+ Game(w, b, byColorPoints)
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
        PlayerGames(player, games.map(_._2))
      .toSeq

  @Benchmark
  def averageOfOpponentsBuchholz(bh: Blackhole) =
    bh.consume:
      allGames.map: pg =>
        Blackhole.consumeCPU(Work)
        Tiebreaker.tb(AverageOfOpponentsBuchholz, pg.player, allGames)

  @Benchmark
  def averagePerfectPerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.map: pg =>
        Blackhole.consumeCPU(Work)
        Tiebreaker.tb(AveragePerfectPerformanceOfOpponents, pg.player, allGames)

  @Benchmark
  def directEncounter(bh: Blackhole) =
    bh.consume:
      allGames.map: pg =>
        Blackhole.consumeCPU(Work)
        Tiebreaker.tb(DirectEncounter, pg.player, allGames)

  @Benchmark
  def perfectTournamentPerformance(bh: Blackhole) =
    bh.consume:
      allGames.map: pg =>
        Blackhole.consumeCPU(Work)
        Tiebreaker.tb(PerfectTournamentPerformance, pg.player, allGames)

  @Benchmark
  def sonnebornBerger(bh: Blackhole) =
    bh.consume:
      allGames.map: pg =>
        Blackhole.consumeCPU(Work)
        Tiebreaker.tb(SonnebornBerger, pg.player, allGames)
