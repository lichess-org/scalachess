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
| Average of Opponents' Buchholz                      | CC   | 8.2     | AOB     |       | ✅ (*3)
| Average Perfect [Tournament] Performance of Opponents| DC  | 10.5    | APPO    |       | ✅
| Average [Tournament] Performance Rating of Opponents | DC  | 10.4    | APRO    |       | ✅
| Average Rating of Opponents                         | D    | 10.1    | ARO     |   ●   | ✅
| Buchholz                                            | C    | 8.1     | BH      |   ●   | ✅ (*3)
| Direct Encounter                                    | A    | 6       | DE      |       | ✅ (*1)
| Fore Buchholz                                       | D    | 8.3     | FB      |   ●   | ✅ (*2, *3)
| Games one Elected to Play                           | B    | 7.6     | GE      |       | ❌ // Need byes
| Koya System for Round Robin                         | BC   | 9.2     | KS      |       | ✅
| Number of Games Played with Black                   | B    | 7.3     | BPG     |       | ✅
| Number of Games Won                                 | B    | 7.2     | WON     |       | ✅
| Number of Games Won with Black                      | B    | 7.4     | BWG     |       | ✅
| Number of Wins                                      | B    | 7.1     | WIN     |       | ❌ // Need byes
| Perfect Tournament Performance                      | DB   | 10.3    | PTP     |       | ✅
| Sonneborn-Berger                                    | BC   | 9.1     | SB      |   ●   | ✅ (*3)
| (Sum of) Progressive Scores                         | B    | 7.5     | PS      |   ●   | ✅
| Tournament Performance Rating                       | DB   | 10.2    | TPR     |       | ✅

1. DE - Not implemented this clause:
  If the tied participants have not played all the games against each other,
  but one of them will be alone at the top of the separate standings whatever the outcome of the missing games,
  that participant is ranked first among the tied participants –
  the same applies to the second rank when the first is assigned this way; and so on.

2. FB -  We don't know which round is the last one -
  so we assume that all players with the maximum number of games are always playing their last round.
  And this will eventually turn out to be true.
  This is not strictly correct and it should be tweaked/removed if it becomes an issue.

3. BH, FB, AOB and SB -
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
  def cutSum(cut: Int): TieBreakPoints =
    tieBreakSeq.sorted.drop(cut).sum

  def average: TieBreakPoints =
    tieBreakSeq.nonEmpty.so:
      tieBreakSeq.sum / tieBreakSeq.size

extension (games: Seq[Tiebreaker.Game])
  def score: TournamentScore =
    games.flatMap(_.points.map(_.value)).sum

import Tiebreaker.*
case object NbBlackGames extends Tiebreaker("BPG", "Number of games played with black"):
  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackGames = tour.gamesById(player.id).filter(_.color == Color.Black).size
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, TieBreakPoints(myBlackGames)))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myGames = allPlayers.get(me.id).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.filter(_.color == Color.Black).size)

case object NbWins extends Tiebreaker("WON", "Number of Wins"):
  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myWins = tour.gamesById(player.id).count(_.points.contains(Points.One))
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, TieBreakPoints(myWins)))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myGames = allPlayers.get(me.id).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.count(_.points.contains(Points.One)))

case object NbBlackWins extends Tiebreaker("BWG", "Number of wins with black"):
  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myBlackWins =
          tour.gamesById(player.id).count(g => g.color == Color.Black && g.points.contains(Points.One))
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, TieBreakPoints(myBlackWins)))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myGames = allPlayers.get(me.id).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.count(g => g.color == Color.Black && g.points.contains(Points.One)))

case object SonnebornBerger extends Tiebreaker("SB", "Sonneborn-Berger score"):
  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ Point(self, tour.sonnebornBergerSeq(player.id).sum))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames  = allPlayers.get(me.id)
    val myOpponents =
      allMyGames.map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    allMyGames.fold(TieBreakPoints(0f)): allMyGames =>
      sonnebornBergerCutN(0, allMyGames, allMyOpponentsGames.toSeq)

case object SonnebornBergerCut1 extends Tiebreaker("SB-C1", "Sonneborn-Berger cut 1"):
  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        player.id -> (previousPoints
          .getOrElse(player.id, Nil) :+ Point(self, tour.sonnebornBergerSeq(player.id).drop(1).sum))
      .toMap
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames  = allPlayers.get(me.id)
    val myOpponents =
      allMyGames.map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    allMyGames.fold(TieBreakPoints(0f)): allMyGames =>
      sonnebornBergerCutN(1, allMyGames, allMyOpponentsGames.toSeq)

case class Buchholz(modifier: Option[Modifier] = None)
    extends Tiebreaker(
      s"BH${modifier.fold("")(m => s"-${m.code}")}",
      s"Buchholz${modifier.fold("")(m => s" ${m.name}")}"
    )
    with CuttableTiebreaker:
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points                    = tour.buchholzSeq(player.id)
        val cutPoints: TieBreakPoints = modifier
          .fold(points):
            case Modifier.Cut1    => points.drop(1)
            case Modifier.Cut2    => points.drop(2)
            case Modifier.Median1 => points.drop(1).dropRight(1)
            case Modifier.Median2 => points.drop(2).dropRight(2)
          .sum
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(this, cutPoints))
      .toMap
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    buchholzCutN(1, allMyOpponentsGames.toSeq)

case object ForeBuchholz extends Tiebreaker("FB", "Fore Buchholz"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    foreBuchholzCutN(0, allMyOpponentsGames.toSeq)

case object ForeBuchholzCut1 extends Tiebreaker("FB-C1", "Fore Buchholz cut 1"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    foreBuchholzCutN(1, allMyOpponentsGames.toSeq)

case object AverageOfOpponentsBuchholz extends Tiebreaker("AOB", "Average of opponents Buchholz score"):

  self =>
  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.opponentsOf(player.id).map(opp => tour.buchholz(opp.id)).average
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    allMyOpponentsGames
      .map: opp =>
        Buchholz().compute(opp.player, allPlayers, previousPoints)
      .toSeq
      .average

case object DirectEncounter extends Tiebreaker("DE", "Direct encounter"):
  self =>

  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myScore    = tour.scoreOf(player.id)
        val tiedWithMe = tour.players.filter(p =>
          tour.scoreOf(p.id) == myScore &&
            previousPoints.get(p.id) == previousPoints.get(player.id)
        )
        val tiedPlayerSet         = tiedWithMe.toSet.excl(player)
        val allTiedPlayersHaveMet = tiedPlayerSet.subsetOf(tour.opponentsOf(player.id).toSet)

        val points =
          if !allTiedPlayersHaveMet then TieBreakPoints(0f)
          else
            val directGames =
              tour.gamesById(player.id).filter(g => tiedPlayerSet.contains(g.opponent))
            if directGames.isEmpty then TieBreakPoints(0f)
            else
              TieBreakPoints:
                directGames
                  .groupBy(_.opponent)
                  .map: (_, games) =>
                    // If the players meet more than once, FIDE says that we average the score
                    games.score.value / games.size
                  .sum
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames = allPlayers.get(me.id)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerWithGames(_, myGames) =>
        val myScore    = myGames.score
        val tiedWithMe = allPlayers.values.filter(p =>
          p.games.score == myScore &&
            previousPoints.get(p.player.id) == previousPoints.get(me.id)
        )
        val tiedPlayerSet         = tiedWithMe.map(_.player).toSet
        val allTiedPlayersHaveMet = tiedWithMe.forall: tied =>
          tiedPlayerSet.excl(tied.player).subsetOf(tied.games.map(_.opponent).toSet)
        if !allTiedPlayersHaveMet then TieBreakPoints(0f)
        else
          val directGames = myGames.filter(g => tiedPlayerSet.contains(g.opponent))
          if directGames.isEmpty then TieBreakPoints(0f)
          else
            TieBreakPoints:
              directGames
                .groupBy(_.opponent)
                .map: (_, games) =>
                  // If the players meet more than once, FIDE says that we average the score
                  games.score.value / games.size
                .sum

case object AverageRatingOfOpponents extends Tiebreaker("ARO", "Average rating of opponents"):

  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myOpponents = tour.opponentsOf(player.id)
        val points      = averageRatingOfOpponentsCutN(0, myOpponents)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(AverageRatingOfOpponents, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents = allPlayers.get(me.id).map(_.games.map(_.opponent)).getOrElse(Nil)
    averageRatingOfOpponentsCutN(0, myOpponents)

case object AverageRatingOfOpponentsCut1 extends Tiebreaker("ARO-C1", "Average rating of opponents cut 1"):

  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val myOpponents = tour.opponentsOf(player.id)
        val points      = averageRatingOfOpponentsCutN(1, myOpponents)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(AverageRatingOfOpponentsCut1, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    averageRatingOfOpponentsCutN(1, allMyOpponentsGames.map(_.player).toSeq)

case object AveragePerformanceOfOpponents extends Tiebreaker("APRO", "Average performance of opponents"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    allMyOpponentsGames
      .map(opp => TournamentPerformanceRating.compute(opp.player, allPlayers, previousPoints))
      .toSeq
      .average
      // FIDE says that the performance rating should be rounded up.
      .map(_.round)

case object KoyaSystem extends Tiebreaker("KS", "Koya system"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames  = allPlayers.get(me.id)
    val myOpponents =
      allMyGames.map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)
    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerWithGames(_, myGames) =>
        val halfOfMaxPossibleScore = TournamentScore(allPlayers.values.map(_.games.size).max / 2f)
        myGames
          .filter: game =>
            allMyOpponentsGames.exists: opponent =>
              game.opponent == opponent.player && opponent.games.score >= halfOfMaxPossibleScore
          .score
          .into(TieBreakPoints)

case object SumOfProgressiveScores extends Tiebreaker("PS", "Sum of progressive scores"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames = allPlayers.get(me.id)
    allMyGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      sumOfProgressiveScoresCutN(0, meAndMyGames)

case object SumOfProgressiveScoresCut1 extends Tiebreaker("PS-C1", "Sum of progressive scores cut 1"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames = allPlayers.get(me.id)
    allMyGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      sumOfProgressiveScoresCutN(1, meAndMyGames)

case object TournamentPerformanceRating extends Tiebreaker("TPR", "Tournament performance rating"):
  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames = allPlayers.get(me.id)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerWithGames(_, myGames) =>
        TieBreakPoints:
          Elo
            .computePerformanceRating:
              myGames.collect:
                case Tiebreaker.Game(Some(points), Tiebreaker.Player(_, Some(rating)), _) =>
                  Elo.Game(points, rating)
            .so(_.value.toFloat)

case object PerfectTournamentPerformance extends Tiebreaker("PTP", "Perfect tournament performance"):
  self =>

  override def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    tour.players.view
      .map: player =>
        val points = tour.perfectTournamentPerformance(player.id)
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val allMyGames = allPlayers.get(me.id)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerWithGames(_, myGames) =>
        val oppRatings: Seq[Int] = myGames.flatMap(_.opponent.rating.map(_.value))
        if oppRatings.isEmpty then TieBreakPoints(0f)
        else
          val myScore = myGames.score
          val minR    = oppRatings.min - 800
          val maxR    = oppRatings.max + 800
          if myScore == TournamentScore(0f) then TieBreakPoints(minR)
          else
            val ptp = binarySearch(oppRatings, myScore)(minR, maxR)
            TieBreakPoints(ptp)

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

case object AveragePerfectPerformanceOfOpponents
    extends Tiebreaker("APPO", "Average perfect tournament performance of opponents"):
  self =>

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
        player.id -> (previousPoints.getOrElse(player.id, Nil) :+ Point(self, points))
      .toMap

  def compute(
      me: Tiebreaker.Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints =
    val myOpponents =
      allPlayers.get(me.id).map(_.games.map(_.opponent).toSet).getOrElse(Set.empty)

    val allMyOpponentsGames = allPlayers.values.filter(pg => myOpponents.contains(pg.player))

    allMyOpponentsGames
      .map: opp =>
        PerfectTournamentPerformance.compute(opp.player, allPlayers, previousPoints)
      .toSeq
      .average
      .map(_.round) // Fide says to round up

type PlayerId = String

import Tiebreaker.*
case class PlayerWithScore(
    player: Player,
    score: Float,
    tiebreakers: List[Tiebreaker.Point]
)

trait Tournament:
  def players: List[Player]
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

  lazy val buchholz: PlayerId => TieBreakPoints = memoize: id =>
    buchholzSeq(id).sum

  // Memoize sorted sequences of cuttable tiebreaks so that we can cut them later in the tournament if necessary
  // Without having to recompute them.
  // Tournaments often include both the cut and the uncut version of the tiebreaker.
  lazy val buchholzSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    opponentsOf(id)
      .map(opponent => scoreOf(opponent.id).into(TieBreakPoints))
      .sorted

  lazy val sonnebornBergerSeq: PlayerId => Seq[TieBreakPoints] = memoize: id =>
    toPlayerGames(id).games
      .map: game =>
        game.points match
          case Some(Points.One)  => scoreOf(game.opponent.id).into(TieBreakPoints)
          case Some(Points.Half) => scoreOf(game.opponent.id).map(_ / 2f).into(TieBreakPoints)
          case _                 => TieBreakPoints(0f)
      .sorted

  given Ordering[Tiebreaker.Point]       = Ordering.by(_.points)
  given Ordering[List[Tiebreaker.Point]] = new Ordering[List[Tiebreaker.Point]]:
    def compare(a: List[Tiebreaker.Point], b: List[Tiebreaker.Point]): Int =
      @scala.annotation.tailrec
      def loop(
          a: List[Tiebreaker.Point],
          b: List[Tiebreaker.Point]
      ): Int =
        (a, b) match
          case (Nil, Nil)           => 0
          case (Nil, _)             => -1 // a is empty, b is not
          case (_, Nil)             => 1  // b is empty, a is not
          case (ah :: at, bh :: bt) =>
            val cmp = ah.points.value.compare(bh.points.value)
            if cmp != 0 then cmp else loop(at, bt)

      loop(a, b)

  given Ordering[PlayerWithScore] = new Ordering[PlayerWithScore]:
    def compare(a: PlayerWithScore, b: PlayerWithScore): Int =
      // sort by score descending, then by tiebreakers descending
      val scoreComparison = b.score.compare(a.score)
      if scoreComparison != 0 then scoreComparison
      else Ordering[List[Tiebreaker.Point]].compare(b.tiebreakers, a.tiebreakers)

  // compute and sort players by their scores and tiebreakers
  def compute(tiebreakers: List[Tiebreaker]): List[PlayerWithScore] =
    val points = tiebreakers.foldLeft(Map.empty[PlayerId, List[Point]]): (acc, tiebreaker) =>
      tiebreaker.compute(this, acc)
    players
      .map: player =>
        PlayerWithScore(player, scoreOf(player.id).value, points.getOrElse(player.id, Nil))
      .sorted

object Tournament:

  private case class Impl(games: Map[PlayerId, Tiebreaker.PlayerWithGames]) extends Tournament:

    override def toPlayerGames: Map[PlayerId, PlayerWithGames] = games

    @scala.annotation.threadUnsafe
    override lazy val players: List[Player] = games.values.map(_.player).toList

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

enum Modifier(val code: String, val name: String):
  case Cut1    extends Modifier("C1", "Cut1")
  case Cut2    extends Modifier("C2", "Cut2")
  case Median1 extends Modifier("M1", "Median1")
  case Median2 extends Modifier("M2", "Median2")

trait CuttableTiebreaker extends Tiebreaker

trait Tiebreaker(val code: String, val name: String):
  self =>
  // compute players' tiebreak points based on the tournament and a list of previously computed tiebreak points
  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    val playerGames = tour.toPlayerGames
    playerGames.values
      .map: pg =>
        val point = Point(self, self.compute(pg.player, playerGames, previousPoints))
        pg.player.id -> (previousPoints.getOrElse(pg.player.id, Nil) :+ point)
      .toMap

  def compute(
      me: Player,
      allPlayers: Map[PlayerId, Tiebreaker.PlayerWithGames],
      previousPoints: PlayerPoints
  ): TieBreakPoints

object Tiebreaker:
  case class Point(tiebreaker: Tiebreaker, points: TieBreakPoints)
  type PlayerPoints = Map[PlayerId, List[Point]]

  def compute(
      games: Map[PlayerId, Tiebreaker.PlayerWithGames],
      tiebreakers: List[Tiebreaker]
  ): List[PlayerWithScore] =
    Tournament(games).compute(tiebreakers)

  // old tiebreakers
  case class PlayerWithGames(player: Player, games: Seq[Game])

  case class Game(points: Option[Outcome.Points], opponent: Player, color: Color)

  case class Player(id: PlayerId, rating: Option[Elo])

  val all: List[Tiebreaker] = List(
    NbBlackGames,
    NbWins,
    NbBlackWins,
    SonnebornBerger,
    SonnebornBergerCut1,
    ForeBuchholz,
    ForeBuchholzCut1,
    AverageOfOpponentsBuchholz,
    DirectEncounter,
    AverageRatingOfOpponents,
    AverageRatingOfOpponentsCut1,
    AveragePerformanceOfOpponents,
    KoyaSystem,
    SumOfProgressiveScores,
    SumOfProgressiveScoresCut1,
    TournamentPerformanceRating,
    PerfectTournamentPerformance,
    AveragePerfectPerformanceOfOpponents
  )

  val byCode: Map[String, Tiebreaker] = all.mapBy(_.code)

private def sumOfProgressiveScoresCutN(
    cut: Int,
    player: Tiebreaker.PlayerWithGames
): TieBreakPoints =
  player.games.indices
    .map: i =>
      player.games.take(i + 1).score.into(TieBreakPoints)
    .cutSum(cut)

private def averageRatingOfOpponentsCutN(
    cut: Int,
    opponentGames: Seq[Player]
): TieBreakPoints =
  opponentGames
    .collect:
      case Tiebreaker.Player(_, Some(elo)) => TieBreakPoints(elo.value)
    .sorted
    .drop(cut)
    .average
    .map(_.round) // Must round up according to FIDE rules

private def buchholzCutN(cut: Int, opponentGames: Seq[Tiebreaker.PlayerWithGames]): TieBreakPoints =
  opponentGames
    .map(_.games.score.into(TieBreakPoints))
    .cutSum(cut)

private def foreBuchholzCutN(
    cut: Int,
    opponentGames: Seq[Tiebreaker.PlayerWithGames]
): TieBreakPoints =
  val maxGames            = opponentGames.map(_.games.size).max
  val playersWithMaxGames = opponentGames.filter(_.games.size == maxGames).map(_.player).toSet
  val withLastRoundDrawn: Seq[Tiebreaker.PlayerWithGames] = opponentGames.map: opp =>
    if playersWithMaxGames.contains(opp.player) then
      opp.copy(
        games = opp.games.dropRight(1) ++ opp.games.lastOption.map(_.copy(points = Some(Points.Half))).toSeq
      )
    else opp
  buchholzCutN(cut, withLastRoundDrawn)

private def sonnebornBergerCutN(
    cut: Int,
    player: Tiebreaker.PlayerWithGames,
    opponentGames: Seq[Tiebreaker.PlayerWithGames]
): TieBreakPoints =
  player.games
    .map: game =>
      val oppScore = opponentGames.find(_.player == game.opponent).map(_.games.score)
      (oppScore, game.points) match
        case (Some(score), Some(Points.One))  => score.into(TieBreakPoints)
        case (Some(score), Some(Points.Half)) => score.map(_ / 2f).into(TieBreakPoints)
        case _                                => TieBreakPoints(0f)
    .cutSum(cut)

private def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))
