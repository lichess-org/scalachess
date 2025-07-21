package chess
package tiebreak

import cats.Applicative
import cats.syntax.all.*
import chess.Outcome.Points
import chess.rating.Elo
import scalalib.extensions.*
import scalalib.newtypes.*
import scalalib.zeros.given

import Tiebreak.*

/*
Tie-breakers for individuals for swiss/round-robins
https://handbook.fide.com/chapter/TieBreakRegulations082024

| Name (in alphabetical order)                        | Type | Section | Acronym | Cut-1 |
|-----------------------------------------------------|------|---------|---------|-------|
| Average of Opponents' Buchholz                      | CC   | 8.2     | AOB     |       | ✅ (*2)
| Average Perfect [Tournament] Performance of Opponents| DC  | 10.5    | APPO    |       | ✅
| Average [Tournament] Performance Rating of Opponents | DC  | 10.4    | APRO    |       | ✅
| Average Rating of Opponents                         | D    | 10.1    | ARO     |   ●   | ✅
| Buchholz                                            | C    | 8.1     | BH      |   ●   | ✅ (*2)
| Direct Encounter                                    | A    | 6       | DE      |       | ✅ (*1)
| Fore Buchholz                                       | D    | 8.3     | FB      |   ●   | ✅ (*2)
| Games one Elected to Play                           | B    | 7.6     | GE      |       | ❌ // Need byes
| Koya System for Round Robin                         | BC   | 9.2     | KS      |       | ✅
| Number of Games Played with Black                   | B    | 7.3     | BPG     |       | ✅
| Number of Games Won                                 | B    | 7.2     | WON     |       | ✅
| Number of Games Won with Black                      | B    | 7.4     | BWG     |       | ✅
| Number of Wins                                      | B    | 7.1     | WIN     |       | ❌ // Need byes
| Perfect Tournament Performance                      | DB   | 10.3    | PTP     |       | ✅
| Sonneborn-Berger                                    | BC   | 9.1     | SB      |   ●   | ✅ (*2)
| (Sum of) Progressive Scores                         | B    | 7.5     | PS      |   ●   | ✅
| Tournament Performance Rating                       | DB   | 10.2    | TPR     |       | ✅

1. DE - Not implemented this clause:
  If the tied participants have not played all the games against each other,
  but one of them will be alone at the top of the separate standings whatever the outcome of the missing games,
  that participant is ranked first among the tied participants –
  the same applies to the second rank when the first is assigned this way; and so on.

2. BH, FB, AOB and SB -
  We don't have information about byes and forfeits so we do not implement FIDE's recommendations about
  handling unplayed rounds. (Section 16)
 */

opaque type TiebreakPoint = Float
object TiebreakPoint extends OpaqueFloat[TiebreakPoint]:
  val zero: TiebreakPoint = TiebreakPoint(0f)
given Numeric[TiebreakPoint] = Numeric[Float]

opaque type TournamentScore = Float
object TournamentScore extends OpaqueFloat[TournamentScore]:
  extension (score: TournamentScore) def >=(other: TournamentScore): Boolean = score.value >= other.value

sealed trait Tiebreak(val code: Code, val description: String):
  def extendedCode: String = code
  // compute players' tiebreak points based on the tournament and a list of previously computed tiebreak points
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints

  def foldModifier[B](
      default: => B,
      f: CutModifier => B,
      g: LimitModifier => B
  ): B =
    this match
      case SumOfProgressiveScores(modifier) =>
        f(modifier)
      case AverageRatingOfOpponents(modifier) =>
        f(modifier)
      case SonnebornBerger(modifier) =>
        f(modifier)
      case Buchholz(modifier) =>
        f(modifier)
      case ForeBuchholz(modifier) =>
        f(modifier)
      case KoyaSystem(modifier) =>
        g(modifier)
      case TournamentPerformanceRating | PerfectTournamentPerformance | NbBlackGames | NbWins | NbBlackWins |
          AverageOfOpponentsBuchholz | DirectEncounter | AveragePerformanceOfOpponents |
          AveragePerfectPerformanceOfOpponents =>
        default

case object NbBlackGames extends Tiebreak("BPG", "Number of games played with black"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackGames = tour.gamesById(player.id).filter(_.color == Color.Black).size
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TiebreakPoint(myBlackGames))
      .toMap

case object NbWins extends Tiebreak("WON", "Number of Wins"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myWins = tour.gamesById(player.id).count(_.points == Points.One)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TiebreakPoint(myWins))
      .toMap

case object NbBlackWins extends Tiebreak("BWG", "Number of wins with black"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackWins =
          tour.gamesById(player.id).count(g => g.color == Color.Black && g.points == Points.One)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TiebreakPoint(myBlackWins))
      .toMap

case class SonnebornBerger(modifier: CutModifier)
    extends Tiebreak("SB", modifier.extendedDescription("Sonneborn-Berger")):
  override def extendedCode: String = modifier.extendedCode(code)
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ tour.sonnebornBergerSeq(player.id).cutSum(modifier))
      .toMap

case class Buchholz(modifier: CutModifier) extends Tiebreak("BH", modifier.extendedDescription("Buchholz")):
  override def extendedCode: String = modifier.extendedCode(code)
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ tour.buchholzSeq(player.id).cutSum(modifier))
      .toMap

case class ForeBuchholz(modifier: CutModifier)
    extends Tiebreak("FB", modifier.extendedDescription("Fore Buchholz")):
  override def extendedCode: String = modifier.extendedCode(code)
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points: TiebreakPoint = tour.foreBuchholzSeq(player.id).cutSum(modifier)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AverageOfOpponentsBuchholz extends Tiebreak("AOB", "Average of opponents Buchholz score"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.opponentsOf(player.id).map(opp => tour.buchholz(opp.id)).average
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object DirectEncounter extends Tiebreak("DE", "Direct encounter"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    val builder = Map.newBuilder[PlayerId, List[TiebreakPoint]]
    tour.players
      .groupBy(p => (tour.scoreOf(p.id), previousPoints.get(p.id)))
      .foreach: (_, tiedPlayers) =>
        lazy val allTiedPlayersHaveMet = tiedPlayers.forall: player =>
          tiedPlayers.toSet.excl(player).subsetOf(tour.opponentsOf(player.id).toSet)
        tiedPlayers.foreach: player =>
          val points =
            if tiedPlayers.size <= 1 || !allTiedPlayersHaveMet then TiebreakPoint.zero
            else
              val directGames =
                tour.gamesById(player.id).filter(g => tiedPlayers.toSet.excl(player).contains(g.opponent))
              if directGames.isEmpty then TiebreakPoint.zero
              else
                TiebreakPoint:
                  directGames
                    .groupBy(_.opponent)
                    .map: (_, games) =>
                      // If the players meet more than once, FIDE says that we average the score
                      games.score.value / games.size
                    .sum
          builder.addOne(player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points))
    builder.result()

case class AverageRatingOfOpponents(modifier: CutModifier)
    extends Tiebreak("ARO", modifier.extendedDescription("Average rating of opponents")):
  override def extendedCode: String = modifier.extendedCode(code)
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour
          .opponentsOf(player.id)
          .collect:
            case Tiebreak.Player(_, Some(elo)) => TiebreakPoint(elo.value)
          .sorted
          .cut(modifier)
          .average
          .map(_.round) // Fide says to round up
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AveragePerformanceOfOpponents extends Tiebreak("APRO", "Average performance of opponents"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myOpponents = tour.opponentsOf(player.id)
        val points = myOpponents.map(opp => tour.tournamentPerformance(opp.id)).average
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case class KoyaSystem(val limit: LimitModifier)
    extends Tiebreak("KS", s"Koya system (limit ${(limit.value * 100).toInt}% of score)"):
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myGames = tour.gamesById(player.id)
        val maxPossibleScoreWithLimit = tour.maxRounds * limit.value
        val points = myGames
          .filter: game =>
            tour.scoreOf(game.opponent.id).value >= maxPossibleScoreWithLimit
          .score
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points.into(TiebreakPoint))
      .toMap

case class SumOfProgressiveScores(modifier: CutModifier)
    extends Tiebreak("PS", modifier.extendedDescription("Sum of progressive scores")):
  override def extendedCode: String = modifier.extendedCode(code)
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val pointsSeq = tour.progressiveScoresSeq(player.id)
        val points: TiebreakPoint = pointsSeq.cut(modifier).sum
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object TournamentPerformanceRating extends Tiebreak("TPR", "Tournament performance rating"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ tour.tournamentPerformance(player.id))
      .toMap

case object PerfectTournamentPerformance extends Tiebreak("PTP", "Perfect tournament performance"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.perfectTournamentPerformance(player.id)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AveragePerfectPerformanceOfOpponents
    extends Tiebreak("APPO", "Average perfect tournament performance of opponents"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour
          .opponentsOf(player.id)
          .map: opp =>
            tour.perfectTournamentPerformance(opp.id)
          .toSeq
          .average
          .map(_.round) // Fide says to round up
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

type PlayerId = String

import Tiebreak.*

trait Tournament:
  def players: Set[Player]
  def gamesById(id: PlayerId): List[Game]
  def pointsById(id: PlayerId): Option[Float]
  def toPlayerGames: Map[PlayerId, Tiebreak.PlayerWithGames]
  def opponentsOf: PlayerId => List[Player]
  def scoreOf: PlayerId => TournamentScore
  // def currentRound: Int
  // def totalRounds: Int
  // def byes: (PlayerId, Int) => Boolean // playerId, round => true if player has a bye in that round

  lazy val perfectTournamentPerformance: PlayerId => TiebreakPoint = memoize: id =>
    val oppRatings: Seq[Int] =
      gamesById(id).flatMap(_.opponent.rating.map(_.value))
    if oppRatings.isEmpty then TiebreakPoint.zero
    else
      val myScore = scoreOf(id)
      val minR = oppRatings.min - 800
      val maxR = oppRatings.max + 800
      if myScore == TournamentScore(0f) then TiebreakPoint(minR)
      else TiebreakPoint(Tournament.binarySearch(oppRatings, myScore)(minR, maxR))

  lazy val tournamentPerformance: PlayerId => TiebreakPoint = memoize: id =>
    val myGames = gamesById(id)
    TiebreakPoint:
      Elo
        .computePerformanceRating:
          myGames
            .collect:
              case Tiebreak.Game(points, Tiebreak.Player(_, Some(rating)), _, _) =>
                Elo.Game(points, rating)
        .so(_.value.toFloat)

  lazy val maxRounds = players.map(p => gamesById(p.id).size).maxOption.getOrElse(0)

  lazy val buchholz: PlayerId => TiebreakPoint = memoize: id =>
    buchholzSeq(id).sum

  // Memoize sorted sequences of cuttable tiebreaks so that we can cut them later in the tournament if necessary
  // Without having to recompute them.
  // Tournaments often include both the cut and the uncut version of the tiebreak.
  lazy val buchholzSeq: PlayerId => Seq[TiebreakPoint] = memoize: id =>
    opponentsOf(id)
      .map(opponent => scoreOf(opponent.id).into(TiebreakPoint))
      .sorted

  lazy val lastRoundId: Option[String] = toPlayerGames.values
    .maxBy(_.games.size)
    .games
    .lastOption
    .flatMap(_.roundId)

  lazy val foreBuchholzSeq: PlayerId => Seq[TiebreakPoint] = memoize: id =>
    lastRoundId.fold(buchholzSeq(id)): lastRound =>
      opponentsOf(id)
        .map: opponent =>
          TiebreakPoint:
            val opponentGames = gamesById(opponent.id)
            val lastRoundGame = opponentGames.find(_.roundId.exists(_ == lastRound))
            if lastRoundGame.isDefined then opponentGames.dropRight(1).score.value + 0.5f
            else opponentGames.score.value
        .sorted

  lazy val sonnebornBergerSeq: PlayerId => Seq[TiebreakPoint] = memoize: id =>
    toPlayerGames(id).games
      .map: game =>
        game.points match
          case Points.One => scoreOf(game.opponent.id).into(TiebreakPoint)
          case Points.Half => scoreOf(game.opponent.id).map(_ / 2f).into(TiebreakPoint)
          case _ => TiebreakPoint.zero
      .sorted

  lazy val progressiveScoresSeq: PlayerId => Seq[TiebreakPoint] = memoize: id =>
    val games = gamesById(id)
    games.indices
      .map: i =>
        games.take(i + 1).score.into(TiebreakPoint)
      .sorted

  given Ordering[List[TiebreakPoint]] = new:
    def compare(a: List[TiebreakPoint], b: List[TiebreakPoint]): Int =
      @scala.annotation.tailrec
      def loop(a: List[TiebreakPoint], b: List[TiebreakPoint]): Int = (a, b) match
        case (Nil, Nil) => 0
        case (Nil, _) => -1 // a is empty, b is not
        case (_, Nil) => 1 // b is empty, a is not
        case (ah :: at, bh :: bt) =>
          val cmp = ah.value.compare(bh.value)
          if cmp != 0 then cmp else loop(at, bt)

      loop(a, b)

  given Ordering[PlayerWithScore] = new Ordering[PlayerWithScore]:
    def compare(a: PlayerWithScore, b: PlayerWithScore): Int =
      // sort by score descending, then by tiebreaks descending
      val scoreComparison = b.score.compare(a.score)
      if scoreComparison != 0 then scoreComparison
      else Ordering[List[TiebreakPoint]].compare(b.tiebreakPoints, a.tiebreakPoints)

  // compute and sort players by their scores and tiebreaks
  def compute(tiebreaks: List[Tiebreak]): List[PlayerWithScore] =
    val points = tiebreaks.foldLeft(Map.empty[PlayerId, List[TiebreakPoint]]): (acc, tiebreaks) =>
      tiebreaks.compute(this, acc)
    players.toList
      .map: player =>
        PlayerWithScore(player, scoreOf(player.id).value, points.getOrElse(player.id, Nil))
      .sorted

object Tournament:

  private case class Impl(games: Map[PlayerId, Tiebreak.PlayerWithGames]) extends Tournament:

    override def toPlayerGames: Map[PlayerId, PlayerWithGames] = games

    override lazy val players: Set[Player] = games.values.map(_.player).toSet

    override lazy val opponentsOf: PlayerId => List[Player] = memoize: id =>
      games.get(id).map(_.games.map(_.opponent).toList).getOrElse(Nil)

    override lazy val scoreOf: PlayerId => TournamentScore = memoize: id =>
      games.get(id).map(_.games.score).getOrElse(TournamentScore(0f))

    override def gamesById(id: PlayerId): List[Game] =
      games.get(id).fold(Nil)(_.games.toList)

    override def pointsById(id: PlayerId): Option[Float] =
      games
        .get(id)
        .map(_.games.map(_.points.value).sum)

  // Find the lowest integer rating R such that sum of expected scores >= myScore
  // Use the full FIDE conversion table, no ±400 cut
  private def expectedScoreFor(r: Int, oppRatings: Seq[Int]) = TournamentScore:
    oppRatings
      .foldMap: oppR =>
        Elo.getExpectedScore(oppR - r)

  @annotation.tailrec
  def binarySearch(oppRatings: Seq[Int], myScore: TournamentScore)(low: Int, high: Int): Int =
    if low >= high then low
    else
      val mid = (low + high) / 2
      if expectedScoreFor(mid, oppRatings) >= myScore then binarySearch(oppRatings, myScore)(low, mid)
      else binarySearch(oppRatings, myScore)(mid + 1, high)

  def apply(games: Map[PlayerId, Tiebreak.PlayerWithGames]): Tournament =
    Impl(games)

enum CutModifier(val code: String, val name: String, val top: Int, val bottom: Int):
  case None extends CutModifier("", "", 0, 0)
  case Cut1 extends CutModifier("C1", "Cut1", 0, 1)
  case Cut2 extends CutModifier("C2", "Cut2", 0, 2)
  case Median1 extends CutModifier("M1", "Median1", 1, 1)
  case Median2 extends CutModifier("M2", "Median2", 2, 2)

  def extendedCode(code: String): String =
    if this == CutModifier.None then code
    else s"$code-${this.code}"

  def extendedDescription(name: String): String =
    if this == CutModifier.None then name
    else s"$name ${this.name}"

object CutModifier:
  val byCode = values.mapBy(_.code)

opaque type LimitModifier = Float
object LimitModifier:
  def apply(value: Float): Option[LimitModifier] =
    if value < 0f || value > 1f then None
    else Some(value)

  val default: LimitModifier = 0.5f

  extension (lm: LimitModifier) inline def value: Float = lm

object Tiebreak:
  type Code = "BPG" | "WON" | "BWG" | "BH" | "FB" | "AOB" | "DE" | "ARO" | "APRO" | "APPO" | "KS" | "TPR" |
    "PTP" | "SB" | "PS"

  object Code:

    //format: off
    private val all: List[Code] = List("BPG", "WON", "BWG", "BH", "FB", "AOB", "DE", "ARO", "APRO", "APPO", "KS", "TPR", "PTP", "SB", "PS")
    //format: on

    val byStr: Map[String, Code] = all.mapBy(_.toString)

  def apply[F[_]: Applicative](
      code: Code,
      mkCutModifier: => F[CutModifier],
      mkLimitModifier: => F[LimitModifier]
  ): F[Tiebreak] =
    code match
      case "BPG" => NbBlackGames.pure[F]
      case "WON" => NbWins.pure[F]
      case "BWG" => NbBlackWins.pure[F]
      case "BH" => mkCutModifier.map(Buchholz.apply)
      case "FB" => mkCutModifier.map(ForeBuchholz.apply)
      case "AOB" => AverageOfOpponentsBuchholz.pure[F]
      case "DE" => DirectEncounter.pure[F]
      case "ARO" => mkCutModifier.map(AverageRatingOfOpponents.apply)
      case "APRO" => AveragePerformanceOfOpponents.pure[F]
      case "APPO" => AveragePerfectPerformanceOfOpponents.pure[F]
      case "KS" => mkLimitModifier.map(KoyaSystem.apply)
      case "TPR" => TournamentPerformanceRating.pure[F]
      case "PTP" => PerfectTournamentPerformance.pure[F]
      case "SB" => mkCutModifier.map(SonnebornBerger.apply)
      case "PS" => mkCutModifier.map(SumOfProgressiveScores.apply)

  def compute(
      games: Map[PlayerId, Tiebreak.PlayerWithGames],
      tiebreaks: List[Tiebreak]
  ): List[PlayerWithScore] =
    Tournament(games).compute(tiebreaks)

  type PlayerPoints = Map[PlayerId, List[TiebreakPoint]]

  case class PlayerWithGames(player: Player, games: Seq[Game])

  case class PlayerWithScore(
      player: Player,
      score: Float,
      tiebreakPoints: List[TiebreakPoint]
  )

  case class Game(points: Outcome.Points, opponent: Player, color: Color, roundId: Option[String])

  case class Player(id: PlayerId, rating: Option[Elo])

  val allSimple: List[Tiebreak] = List(
    NbBlackGames,
    NbWins,
    NbBlackWins,
    AverageOfOpponentsBuchholz,
    DirectEncounter,
    AveragePerformanceOfOpponents,
    KoyaSystem(LimitModifier.default),
    TournamentPerformanceRating,
    PerfectTournamentPerformance,
    AveragePerfectPerformanceOfOpponents
  )

  val allCuttable: Seq[Tiebreak] =
    CutModifier.values.toList.flatMap: modifier =>
      List(
        SonnebornBerger(modifier),
        Buchholz(modifier),
        ForeBuchholz(modifier),
        AverageRatingOfOpponents(modifier),
        SumOfProgressiveScores(modifier)
      )

  val preset: List[Tiebreak] = allSimple ++ allCuttable

private def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))

extension (tieBreakSeq: Seq[TiebreakPoint])
  def cut(modifier: CutModifier): Seq[TiebreakPoint] =
    tieBreakSeq.drop(modifier.bottom).dropRight(modifier.top)

  def cutSum(modifier: CutModifier): TiebreakPoint =
    tieBreakSeq.cut(modifier).sum

  def average: TiebreakPoint =
    tieBreakSeq.nonEmpty.so:
      tieBreakSeq.sum / tieBreakSeq.size

extension (games: Seq[Tiebreak.Game])
  def score: TournamentScore =
    games.map(_.points.value).sum
