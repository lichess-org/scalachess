package chess.tiebreaker

import chess.Color
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
| Average of Opponents' Buchholz                      | CC   | 8.2     | AOB     |       | ✅
| Average Perfect [Tournament] Performance of Opponents| DC  | 10.5    | APPO    |       | ✅
| Average [Tournament] Performance Rating of Opponents | DC  | 10.4    | APRO    |       | ✅
| Average Rating of Opponents                         | D    | 10.1    | ARO     |   ●   | ✅
| Buchholz                                            | C    | 8.1     | BH      |   ●   | ✅
| Direct Encounter                                    | A    | 6       | DE      |       | ✅ (*1)
| Fore Buchholz                                       | D    | 8.3     | FB      |   ●   | ✅ (*2)
| Games one Elected to Play                           | B    | 7.6     | GE      |       | ❌ // Need byes
| Koya System for Round Robin                         | BC   | 9.2     | KS      |       | ✅
| Number of Games Played with Black                   | B    | 7.3     | BPG     |       | ✅
| Number of Games Won                                 | B    | 7.2     | WON     |       | ✅
| Number of Games Won with Black                      | B    | 7.4     | BWG     |       | ✅
| Number of Wins                                      | B    | 7.1     | WIN     |       | ❌ // Need byes
| Perfect Tournament Performance                      | DB   | 10.3    | PTP     |       | ✅
| Sonneborn-Berger                                    | BC   | 9.1     | SB      |   ●   | ✅
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
 */
trait Tiebreaker(val code: String, val name: String):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints

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

extension (games: Seq[Tiebreaker.POVGame])
  def score: TournamentScore =
    games.flatMap(_.points.map(_.value)).sum

case object NbBlackGames extends Tiebreaker("BPG", "Number of games played with black"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val myGames = allPlayers.find(_.player == me).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.filter(_.color == Color.Black).size)

case object NbWins extends Tiebreaker("WON", "Number of Wins"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val myGames = allPlayers.find(_.player == me).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.count(_.points.contains(Points.One)))
case object NbBlackWins extends Tiebreaker("BWG", "Number of wins with black"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val myGames = allPlayers.find(_.player == me).map(_.games).getOrElse(Seq.empty)
    TieBreakPoints(myGames.count(g => g.color == Color.Black && g.points.contains(Points.One)))

case object SonnebornBerger extends Tiebreaker("SB", "Sonneborn-Berger score"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames          = allPlayers.find(_.player == me)
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyGames.fold(TieBreakPoints(0f)): allMyGames =>
      sonnebornBergerCutN(0, allMyGames, allMyOpponentsGames)

case object SonnebornBergerCut1 extends Tiebreaker("SB-C1", "Sonneborn-Berger cut 1"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames          = allPlayers.find(_.player == me)
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyGames.fold(TieBreakPoints(0f)): allMyGames =>
      sonnebornBergerCutN(1, allMyGames, allMyOpponentsGames)

case object Buchholz extends Tiebreaker("BH", "Buchholz score"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    buchholzCutN(0, allMyOpponentsGames)

case object BuchholzCut1 extends Tiebreaker("BH-C1", "Buchholz cut 1"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    buchholzCutN(1, allMyOpponentsGames)

case object BuchholzCut2 extends Tiebreaker("BH-C2", "Buchholz cut 2"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    buchholzCutN(2, allMyOpponentsGames)

case object ForeBuchholz extends Tiebreaker("FB", "Fore Buchholz"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    foreBuchholzCutN(0, allMyOpponentsGames)

case object ForeBuchholzCut1 extends Tiebreaker("FB-C1", "Fore Buchholz cut 1"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    foreBuchholzCutN(1, allMyOpponentsGames)

case object AverageOfOpponentsBuchholz extends Tiebreaker("AOB", "Average of opponents Buchholz score"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyOpponentsGames
      .map: opp =>
        Buchholz.compute(opp.player, allPlayers)
      .average

case object DirectEncounter extends Tiebreaker("DE", "Direct encounter"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames = allPlayers.find(_.player == me)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerGames(_, myGames, partialTiebreaks) =>
        val myScore    = myGames.score
        val tiedWithMe = allPlayers.filter(p =>
          p.games.score == myScore && p.partialTiebreaks
            .zip(partialTiebreaks)
            .forall(_ == _)
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
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    averageRatingOfOpponentsCutN(0, allMyOpponentsGames)

case object AverageRatingOfOpponentsCut1 extends Tiebreaker("ARO-C1", "Average rating of opponents cut 1"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    averageRatingOfOpponentsCutN(1, allMyOpponentsGames)

case object AveragePerformanceOfOpponents extends Tiebreaker("APRO", "Average performance of opponents"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyOpponentsGames
      .map(opp => TournamentPerformanceRating.compute(opp.player, allPlayers))
      .average
      // FIDE says that the performance rating should be rounded up.
      .map(_.round)

case object KoyaSystem extends Tiebreaker("KS", "Koya system"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val halfOfMaxPossibleScore = TournamentScore(allPlayers.map(_.games.size).max / 2f)
    val allMyGames             = allPlayers.find(_.player == me)
    val allMyOpponentsGames    = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerGames(_, myGames, _) =>
        myGames
          .filter: game =>
            allMyOpponentsGames.exists: opponent =>
              game.opponent == opponent.player && opponent.games.score >= halfOfMaxPossibleScore
          .score
          .into(TieBreakPoints)

case object SumOfProgressiveScores extends Tiebreaker("PS", "Sum of progressive scores"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames = allPlayers.find(_.player == me)
    allMyGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      sumOfProgressiveScoresCutN(0, meAndMyGames)

case object SumOfProgressiveScoresCut1 extends Tiebreaker("PS-C1", "Sum of progressive scores cut 1"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames = allPlayers.find(_.player == me)
    allMyGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      sumOfProgressiveScoresCutN(1, meAndMyGames)

case object TournamentPerformanceRating extends Tiebreaker("TPR", "Tournament performance rating"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames = allPlayers.find(_.player == me)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerGames(_, myGames, _) =>
        TieBreakPoints:
          Elo
            .computePerformanceRating:
              myGames.collect:
                case Tiebreaker.POVGame(Some(points), Tiebreaker.Player(_, Some(rating)), _) =>
                  Elo.Game(points, rating)
            .so(_.value.toFloat)

case object PerfectTournamentPerformance extends Tiebreaker("PTP", "Perfect tournament performance"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyGames = allPlayers.find(_.player == me)
    allMyGames.fold(TieBreakPoints(0f)):
      case Tiebreaker.PlayerGames(_, myGames, _) =>
        val oppRatings = myGames.flatMap(_.opponent.rating.map(_.value))
        if oppRatings.isEmpty then TieBreakPoints(0f)
        else
          val myScore = myGames.score
          val minR    = oppRatings.min - 800
          val maxR    = oppRatings.max + 800
          if myScore == TournamentScore(0f) && oppRatings.nonEmpty then TieBreakPoints(minR)
          else
            // Find the lowest integer rating R such that sum of expected scores >= myScore
            // Use the full FIDE conversion table, no ±400 cut
            def expectedScoreFor(r: Int) = TournamentScore:
              oppRatings
                .map: oppR =>
                  Elo.getExpectedScore(oppR - r)
                .sum
            @annotation.tailrec
            def binarySearch(low: Int, high: Int): Int =
              if low >= high then low
              else
                val mid = (low + high) / 2
                if expectedScoreFor(mid) >= myScore then binarySearch(low, mid)
                else binarySearch(mid + 1, high)
            val ptp = binarySearch(minR, maxR)
            TieBreakPoints(ptp)

case object AveragePerfectPerformanceOfOpponents
    extends Tiebreaker("APPO", "Average perfect tournament performance of opponents"):
  def compute(me: Tiebreaker.Player, allPlayers: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyOpponentsGames
      .map: opp =>
        PerfectTournamentPerformance.compute(opp.player, allPlayers)
      .average
      .map(_.round) // Fide says to round up

object Tiebreaker:

  case class POVGame(
      points: Option[Points],
      opponent: Player,
      color: Color
  )

  case class PlayerGames(
      player: Player,
      games: Seq[POVGame],
      partialTiebreaks: List[TieBreakPoints] = Nil
  )

  case class Player(uniqueIdentifier: String, rating: Option[Elo])

  val all: List[Tiebreaker] = List(
    NbBlackGames,
    NbWins,
    NbBlackWins,
    SonnebornBerger,
    SonnebornBergerCut1,
    Buchholz,
    BuchholzCut1,
    BuchholzCut2,
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
    player: Tiebreaker.PlayerGames
): TieBreakPoints =
  player.games.indices
    .map: i =>
      player.games.take(i + 1).score.into(TieBreakPoints)
    .cutSum(cut)

private def averageRatingOfOpponentsCutN(
    cut: Int,
    opponentGames: Seq[Tiebreaker.PlayerGames]
): TieBreakPoints =
  opponentGames
    .collect:
      case Tiebreaker.PlayerGames(Tiebreaker.Player(_, Some(elo)), _, _) => TieBreakPoints(elo.value)
    .sorted
    .drop(cut)
    .average
    .map(_.round) // Must round up according to FIDE rules

private def buchholzCutN(cut: Int, opponentGames: Seq[Tiebreaker.PlayerGames]): TieBreakPoints =
  opponentGames
    .map(_.games.score.into(TieBreakPoints))
    .cutSum(cut)

private def foreBuchholzCutN(
    cut: Int,
    opponentGames: Seq[Tiebreaker.PlayerGames]
): TieBreakPoints =
  val maxGames            = opponentGames.map(_.games.size).max
  val playersWithMaxGames = opponentGames.filter(_.games.size == maxGames).map(_.player).toSet
  val withLastRoundDrawn: Seq[Tiebreaker.PlayerGames] = opponentGames.map: opp =>
    if playersWithMaxGames.contains(opp.player) then
      opp.copy(
        games = opp.games.dropRight(1) ++ opp.games.lastOption.map(_.copy(points = Some(Points.Half))).toSeq
      )
    else opp
  buchholzCutN(cut, withLastRoundDrawn)

private def sonnebornBergerCutN(
    cut: Int,
    player: Tiebreaker.PlayerGames,
    opponentGames: Seq[Tiebreaker.PlayerGames]
): TieBreakPoints =
  player.games
    .map: game =>
      val oppScore = opponentGames.find(_.player == game.opponent).map(_.games.score)
      (oppScore, game.points) match
        case (Some(score), Some(Points.One))  => score.into(TieBreakPoints)
        case (Some(score), Some(Points.Half)) => score.map(_ / 2f).into(TieBreakPoints)
        case _                                => TieBreakPoints(0f)
    .cutSum(cut)
