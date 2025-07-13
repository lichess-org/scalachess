package chess
package tiebreaker

import cats.syntax.all.*
import chess.Outcome.Points
import chess.rating.Elo
import scalalib.extensions.*
import scalalib.newtypes.*
import scalalib.zeros.given

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

opaque type TieBreakPoints = Float
object TieBreakPoints extends OpaqueFloat[TieBreakPoints]
given Numeric[TieBreakPoints] = Numeric[Float]

opaque type TournamentScore = Float
object TournamentScore extends OpaqueFloat[TournamentScore]:
  extension (score: TournamentScore) def >=(other: TournamentScore): Boolean = score.value >= other.value

extension (tieBreakSeq: Seq[TieBreakPoints])
  def cut(modifier: CutModifier): Seq[TieBreakPoints] =
    tieBreakSeq.drop(modifier.bottom).dropRight(modifier.top)

  def cutSum(modifier: CutModifier): TieBreakPoints =
    tieBreakSeq.cut(modifier).sum

  def average: TieBreakPoints =
    tieBreakSeq.nonEmpty.so:
      tieBreakSeq.sum / tieBreakSeq.size

extension (games: Seq[Tiebreaker.Game])
  def score: TournamentScore =
    games.flatMap(_.points.map(_.value)).sum

import Tiebreaker.*
case object NbBlackGames extends Tiebreaker("BPG", "Number of games played with black"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackGames = tour.gamesById(player.id).filter(_.color == Color.Black).size
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TieBreakPoints(myBlackGames))
      .toMap

case object NbWins extends Tiebreaker("WON", "Number of Wins"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myWins = tour.gamesById(player.id).count(_.points.contains(Points.One))
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TieBreakPoints(myWins))
      .toMap

case object NbBlackWins extends Tiebreaker("BWG", "Number of wins with black"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackWins =
          tour.gamesById(player.id).count(g => g.color == Color.Black && g.points.contains(Points.One))
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ TieBreakPoints(myBlackWins))
      .toMap

case class SonnebornBerger(modifier: CutModifier)
    extends Tiebreaker(modifier.extendedCode("SB"), modifier.extendedName("Sonneborn-Berger")):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ tour.sonnebornBergerSeq(player.id).cutSum(modifier))
      .toMap

case class Buchholz(modifier: CutModifier)
    extends Tiebreaker(modifier.extendedCode("BH"), modifier.extendedName("Buchholz")):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ tour.buchholzSeq(player.id).cutSum(modifier))
      .toMap

case class ForeBuchholz(modifier: CutModifier)
    extends Tiebreaker(modifier.extendedCode("FB"), modifier.extendedName("Fore Buchholz")):
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points: TieBreakPoints = tour.foreBuchholzSeq(player.id).cutSum(modifier)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AverageOfOpponentsBuchholz extends Tiebreaker("AOB", "Average of opponents Buchholz score"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.opponentsOf(player.id).map(opp => tour.buchholz(opp.id)).average
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object DirectEncounter extends Tiebreaker("DE", "Direct encounter"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .groupBy(p => (tour.scoreOf(p.id), previousPoints.get(p.id)))
      .flatMap: (_, tiedPlayers) =>
        lazy val allTiedPlayersHaveMet = tiedPlayers.forall: player =>
          tiedPlayers.toSet.excl(player).subsetOf(tour.opponentsOf(player.id).toSet)
        tiedPlayers.map: player =>
          val points =
            if tiedPlayers.size <= 1 || !allTiedPlayersHaveMet then TieBreakPoints(0f)
            else
              val directGames =
                tour.gamesById(player.id).filter(g => tiedPlayers.toSet.excl(player).contains(g.opponent))
              if directGames.isEmpty then TieBreakPoints(0f)
              else
                TieBreakPoints:
                  directGames
                    .groupBy(_.opponent)
                    .map: (_, games) =>
                      // If the players meet more than once, FIDE says that we average the score
                      games.score.value / games.size
                    .sum
          player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case class AverageRatingOfOpponents(modifier: CutModifier)
    extends Tiebreaker(modifier.extendedCode("ARO"), modifier.extendedName("Average rating of opponents")):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour
          .opponentsOf(player.id)
          .collect:
            case Tiebreaker.Player(_, Some(elo)) => TieBreakPoints(elo.value)
          .sorted
          .cut(modifier)
          .average
          .map(_.round) // Fide says to round up
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AveragePerformanceOfOpponents extends Tiebreaker("APRO", "Average performance of opponents"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myOpponents = tour.opponentsOf(player.id)
        val points      = myOpponents.map(opp => tour.tournamentPerformance(opp.id)).average
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case class KoyaSystem(val limit: LimitModifier)
    extends Tiebreaker(
      s"KS-${(limit.value * 100).toInt}",
      s"Koya system (limit ${(limit.value * 100).toInt}% of score)"
    ):
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myGames                   = tour.gamesById(player.id)
        val maxPossibleScoreWithLimit = tour.maxRounds * limit.value
        val points                    = myGames
          .filter: game =>
            tour.scoreOf(game.opponent.id).value >= maxPossibleScoreWithLimit
          .score
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points.into(TieBreakPoints))
      .toMap

case class SumOfProgressiveScores(modifier: CutModifier)
    extends Tiebreaker(
      modifier.extendedCode("PS"),
      modifier.extendedName("Sum of progressive scores")
    ):
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val pointsSeq              = tour.progressiveScoresSeq(player.id)
        val points: TieBreakPoints = pointsSeq.cut(modifier).sum
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object TournamentPerformanceRating extends Tiebreaker("TPR", "Tournament performance rating"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ tour.tournamentPerformance(player.id))
      .toMap

case object PerfectTournamentPerformance extends Tiebreaker("PTP", "Perfect tournament performance"):
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.perfectTournamentPerformance(player.id)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ points)
      .toMap

case object AveragePerfectPerformanceOfOpponents
    extends Tiebreaker("APPO", "Average perfect tournament performance of opponents"):
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

import Tiebreaker.*

trait Tournament:
  def players: Set[Player]
  def gamesById(id: PlayerId): List[Game]
  def pointsById(id: PlayerId): Option[Float]
  def toPlayerGames: Map[PlayerId, Tiebreaker.PlayerWithGames]
  def opponentsOf: PlayerId => List[Player]
  def scoreOf: PlayerId => TournamentScore
  // def currentRound: Int
  // def totalRounds: Int
  // def byes: (PlayerId, Int) => Boolean // playerId, round => true if player has a bye in that round

  lazy val perfectTournamentPerformance: PlayerId => TieBreakPoints = memoize: id =>
    val oppRatings: Seq[Int] =
      gamesById(id).flatMap(_.opponent.rating.map(_.value))
    if oppRatings.isEmpty then TieBreakPoints(0f)
    else
      val myScore = scoreOf(id)
      val minR    = oppRatings.min - 800
      val maxR    = oppRatings.max + 800
      if myScore == TournamentScore(0f) then TieBreakPoints(minR)
      else TieBreakPoints(Tournament.binarySearch(oppRatings, myScore)(minR, maxR))

  lazy val tournamentPerformance: PlayerId => TieBreakPoints = memoize: id =>
    val myGames = gamesById(id)
    TieBreakPoints:
      Elo
        .computePerformanceRating:
          myGames
            .collect:
              case Tiebreaker.Game(Some(points), Tiebreaker.Player(_, Some(rating)), _, _) =>
                Elo.Game(points, rating)
        .so(_.value.toFloat)

  lazy val maxRounds = players.map(p => gamesById(p.id).size).maxOption.getOrElse(0)

  lazy val buchholz: PlayerId => TieBreakPoints = memoize: id =>
    buchholzSeq(id).sum

  // Memoize sorted sequences of cuttable tiebreaks so that we can cut them later in the tournament if necessary
  // Without having to recompute them.
  // Tournaments often include both the cut and the uncut version of the tiebreaker.
  lazy val buchholzSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    opponentsOf(id)
      .map(opponent => scoreOf(opponent.id).into(TieBreakPoints))
      .sorted

  lazy val lastRoundId: Option[String] = toPlayerGames.values
    .maxBy(_.games.size)
    .games
    .lastOption
    .flatMap(_.roundId)

  lazy val foreBuchholzSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    lastRoundId.fold(buchholzSeq(id)): lastRound =>
      opponentsOf(id)
        .map: opponent =>
          TieBreakPoints:
            val opponentGames = gamesById(opponent.id)
            val lastRoundGame = opponentGames.find(_.roundId.exists(_ == lastRound))
            if lastRoundGame.exists(_.points.isDefined) then opponentGames.dropRight(1).score.value + 0.5f
            else opponentGames.score.value
        .sorted

  lazy val sonnebornBergerSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    toPlayerGames(id).games
      .map: game =>
        game.points match
          case Some(Points.One)  => scoreOf(game.opponent.id).into(TieBreakPoints)
          case Some(Points.Half) => scoreOf(game.opponent.id).map(_ / 2f).into(TieBreakPoints)
          case _                 => TieBreakPoints(0f)
      .sorted

  lazy val progressiveScoresSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    val games = gamesById(id)
    games.indices
      .map: i =>
        games.take(i + 1).score.into(TieBreakPoints)
      .sorted

  given Ordering[List[TieBreakPoints]] = new:
    def compare(a: List[TieBreakPoints], b: List[TieBreakPoints]): Int =
      @scala.annotation.tailrec
      def loop(a: List[TieBreakPoints], b: List[TieBreakPoints]): Int = (a, b) match
        case (Nil, Nil)           => 0
        case (Nil, _)             => -1 // a is empty, b is not
        case (_, Nil)             => 1  // b is empty, a is not
        case (ah :: at, bh :: bt) =>
          val cmp = ah.value.compare(bh.value)
          if cmp != 0 then cmp else loop(at, bt)

      loop(a, b)

  given Ordering[PlayerWithScore] = new Ordering[PlayerWithScore]:
    def compare(a: PlayerWithScore, b: PlayerWithScore): Int =
      // sort by score descending, then by tiebreakers descending
      val scoreComparison = b.score.compare(a.score)
      if scoreComparison != 0 then scoreComparison
      else Ordering[List[TieBreakPoints]].compare(b.tiebreakers, a.tiebreakers)

  // compute and sort players by their scores and tiebreakers
  def compute(tiebreakers: List[Tiebreaker]): List[PlayerWithScore] =
    val points = tiebreakers.foldLeft(Map.empty[PlayerId, List[TieBreakPoints]]): (acc, tiebreaker) =>
      tiebreaker.compute(this, acc)
    players.toList
      .map: player =>
        PlayerWithScore(player, scoreOf(player.id).value, points.getOrElse(player.id, Nil))
      .sorted

object Tournament:

  private case class Impl(games: Map[PlayerId, Tiebreaker.PlayerWithGames]) extends Tournament:

    override def toPlayerGames: Map[PlayerId, PlayerWithGames] = games

    @scala.annotation.threadUnsafe
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
        .map(_.games.flatten(using _.points.map(_.value)).sum)

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

  def apply(games: Map[PlayerId, Tiebreaker.PlayerWithGames]): Tournament =
    Impl(games)

enum CutModifier(val code: String, val name: String, val top: Int, val bottom: Int):
  case None    extends CutModifier("", "", 0, 0)
  case Cut1    extends CutModifier("C1", "Cut1", 0, 1)
  case Cut2    extends CutModifier("C2", "Cut2", 0, 2)
  case Median1 extends CutModifier("M1", "Median1", 1, 1)
  case Median2 extends CutModifier("M2", "Median2", 2, 2)

  def extendedCode(code: String): String =
    if this == CutModifier.None then code
    else s"$code-${this.code}"

  def extendedName(name: String): String =
    if this == CutModifier.None then name
    else s"$name ${this.name}"

opaque type LimitModifier = Float
object LimitModifier extends OpaqueFloat[LimitModifier]

trait Tiebreaker(val code: String, val name: String):
  // compute players' tiebreak points based on the tournament and a list of previously computed tiebreak points
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints

object Tiebreaker:
  type PlayerPoints = Map[PlayerId, List[TieBreakPoints]]

  def compute(
      games: Map[PlayerId, Tiebreaker.PlayerWithGames],
      tiebreakers: List[Tiebreaker]
  ): List[PlayerWithScore] =
    Tournament(games).compute(tiebreakers)

  case class PlayerWithGames(player: Player, games: Seq[Game])

  case class PlayerWithScore(
      player: Player,
      score: Float,
      tiebreakers: List[TieBreakPoints]
  )

  case class Game(points: Option[Outcome.Points], opponent: Player, color: Color, roundId: Option[String])

  case class Player(id: PlayerId, rating: Option[Elo])

  val allSimple: List[Tiebreaker] = List(
    NbBlackGames,
    NbWins,
    NbBlackWins,
    AverageOfOpponentsBuchholz,
    DirectEncounter,
    AveragePerformanceOfOpponents,
    KoyaSystem(LimitModifier(0.5f)),
    TournamentPerformanceRating,
    PerfectTournamentPerformance,
    AveragePerfectPerformanceOfOpponents
  )

  val allCuttable: Seq[Tiebreaker] =
    CutModifier.values.toList.flatMap: modifier =>
      List(
        SonnebornBerger(modifier),
        Buchholz(modifier),
        ForeBuchholz(modifier),
        AverageRatingOfOpponents(modifier),
        SumOfProgressiveScores(modifier)
      )

  val all: List[Tiebreaker] = allSimple ++ allCuttable

  val byCode: Map[String, Tiebreaker] = all.mapBy(_.code)

private def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))
