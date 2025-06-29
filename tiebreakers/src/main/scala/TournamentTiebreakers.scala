package chess.tiebreakers

import chess.rating.Elo
import chess.Outcome.Points
import chess.Color
import scalalib.newtypes.*
import scalalib.extensions.*
import cats.data.NonEmptySeq
/*

Tie-breakers for individuals for swiss/round-robins
https://handbook.fide.com/chapter/TieBreakRegulations082024

| Name (in alphabetical order)                        | Type | Section | Acronym | Cut-1 |
|-----------------------------------------------------|------|---------|---------|-------|
| Average of Opponents' Buchholz                      | CC   | 8.2     | AOB     |       | ✅
| Average Perfect [Tournament] Performance of Opponents| DC   | 10.5    | APPO    |       | ❌
| Average [Tournament] Performance Rating of Opponents | DC   | 10.4    | APRO    |       | ✅
| Average Rating of Opponents                         | D    | 10.1    | ARO     |   ●   | ✅
| Buchholz                                            | C    | 8.1     | BH      |   ●   | ✅
| Direct Encounter                                    | A    | 6       | DE      |       | ✅
| Fore Buchholz                                       | D    | 8.3     | FB      |   ●   | ❌
| Games one Elected to Play                           | B    | 7.6     | GE      |       | ❌
| Koya System for Round Robin                         | BC   | 9.2     | KS      |       | ✅
| Number of Games Played with Black                   | B    | 7.3     | BPG     |       | ✅
| Number of Games Won                                 | B    | 7.2     | WON     |       | ❌
| Number of Games Won with Black                      | B    | 7.4     | BWG     |       | ✅
| Number of Wins                                      | B    | 7.1     | WIN     |       | ✅
| Perfect Tournament Performance                      | DB   | 10.3    | PTP     |       | ✅
| Sonneborn-Berger                                    | BC   | 9.1     | SB      |   ●   | ✅
| (Sum of) Progressive Scores                         | B    | 7.5     | PS      |   ●   | ✅
| Tournament Performance Rating                       | DB   | 10.2    | TPR     |       | ✅
 */
enum Tiebreaker(val code: String, val name: String):

  case NbBlackGames extends Tiebreaker("BPG", "Number of games played with black")

  case NbWins extends Tiebreaker("WIN", "Number of Wins")

  case NbBlackWins extends Tiebreaker("BWG", "Number of wins with black")

  case SonnebornBerger extends Tiebreaker("SB", "Sonneborn-Berger score")

  case SonnebornBergerCut1 extends Tiebreaker("SB-C1", "Sonneborn-Berger cut 1")

  case Buchholz extends Tiebreaker("BH", "Buchholz score")

  case BuchholzCut1 extends Tiebreaker("BH-C1", "Buchholz cut 1")

  case BuchholzCut2 extends Tiebreaker("BH-C2", "Buchholz cut 2")

  case AverageOfOpponentsBuchholz extends Tiebreaker("AOB", "Average of opponents Buchholz score")

  case DirectEncounter extends Tiebreaker("DE", "Direct encounter")

  case AverageRatingOfOpponents extends Tiebreaker("ARO", "Average rating of opponents")

  case AverageRatingOfOpponentsCut1 extends Tiebreaker("ARO-C1", "Average rating of opponents cut 1")

  case AveragePerformanceOfOpponents extends Tiebreaker("APRO", "Average performance of opponents")

  case KoyaSystem extends Tiebreaker("KS", "Koya system")

  case SumOfProgressiveScores extends Tiebreaker("PS", "Sum of progressive scores")

  case SumOfProgressiveScoresCut1 extends Tiebreaker("PS-C1", "Sum of progressive scores cut 1")

  case TournamentPerformanceRating extends Tiebreaker("TPR", "Tournament performance rating")

  case PerfectTournamentPerformance extends Tiebreaker("PTP", "Perfect tournament performance")

opaque type TieBreakPoints = Float
object TieBreakPoints extends OpaqueFloat[TieBreakPoints]
given Numeric[TieBreakPoints] = Numeric[Float]

extension (tieBreakSeq: Seq[TieBreakPoints])
  def cutSum(cut: Int): TieBreakPoints =
    tieBreakSeq.sorted.drop(cut).sum

extension (games: Seq[Tiebreaker.POVGame])
  def score: Float =
    games.flatMap(_.points.map(_.value)).sum // including byes

object Tiebreaker:

  val byCode: Map[String, Tiebreaker] = values.mapBy(_.code)

  private def buchholzCutN(cut: Int, opponentGames: Seq[PlayerGames]): TieBreakPoints =
    opponentGames
      .map: opponent =>
        TieBreakPoints(opponent.games.score)
      .cutSum(cut)

  private def sonnebornBergerCutN(
      cut: Int,
      player: PlayerGames,
      opponentGames: Seq[PlayerGames]
  ): TieBreakPoints =
    player.games
      .map: game =>
        val oppScore = opponentGames.find(_.player == game.opponent).map(_.games.score)
        (oppScore, game.points) match
          case (Some(score), Some(Points.One))  => TieBreakPoints(score)
          case (Some(score), Some(Points.Half)) => TieBreakPoints(score / 2f)
          case _                                => TieBreakPoints(0f)
      .cutSum(cut)

  private def average(numerator: TieBreakPoints, denominator: Float): TieBreakPoints =
    if denominator > 0 then TieBreakPoints(numerator.value / denominator)
    else TieBreakPoints(0f)

  private def averageRatingOfOpponentsCutN(
      cut: Int,
      opponentGames: Seq[PlayerGames]
  ): TieBreakPoints =
    average(
      opponentGames
        .collect:
          case PlayerGames(Player(_, Some(elo)), _, _) => TieBreakPoints(elo.value)
        .cutSum(cut),
      opponentGames.size.toFloat
    ).map(_.round) // Must round up according to FIDE rules

  private def sumOfProgressiveScoresCutN(
      cut: Int,
      player: PlayerGames
  ): TieBreakPoints =
    player.games.indices
      .map: i =>
        TieBreakPoints(player.games.take(i + 1).score)
      .cutSum(cut)

  def tb(tiebreaker: Tiebreaker, me: Player, allPlayers: Seq[PlayerGames]): TieBreakPoints =
    val allMyGames          = allPlayers.find(_.player == me)
    val allMyOpponentsGames = allPlayers.filter(_.games.exists(_.opponent == me))
    allMyGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      meAndMyGames match
        case PlayerGames(_, myGames, partialTiebreaks) =>
          tiebreaker match
            case NbBlackGames => TieBreakPoints(myGames.filter(_.color == Color.Black).size)
            case NbWins       => TieBreakPoints(myGames.count(_.points.contains(Points.One)))
            case NbBlackWins  =>
              TieBreakPoints(myGames.count(g => g.color == Color.Black && g.points.contains(Points.One)))
            case SonnebornBerger            => sonnebornBergerCutN(0, meAndMyGames, allMyOpponentsGames)
            case SonnebornBergerCut1        => sonnebornBergerCutN(1, meAndMyGames, allMyOpponentsGames)
            case Buchholz                   => buchholzCutN(0, allMyOpponentsGames)
            case BuchholzCut1               => buchholzCutN(1, allMyOpponentsGames)
            case BuchholzCut2               => buchholzCutN(2, allMyOpponentsGames)
            case AverageOfOpponentsBuchholz =>
              average(
                allMyOpponentsGames
                  .map: opp =>
                    tb(Buchholz, opp.player, allPlayers)
                  .sum,
                allMyOpponentsGames.size.toFloat
              )
            case DirectEncounter =>
              TieBreakPoints(
                myGames
                  .groupBy(_.opponent)
                  .map: (opponent, games) =>
                    val opponentGames        = allMyOpponentsGames.find(_.player == opponent)
                    val validDirectEncounter =
                      opponentGames.exists(_.games.score == myGames.score) && partialTiebreaks
                        .zip(opponentGames.flatMap(_.partialTiebreaks))
                        .forall(_ == _)
                    if !validDirectEncounter then 0f
                    // If the players meet more than once, FIDE dictates that we average the score
                    else if games.size > 1 then games.score / games.size.toFloat
                    else games.score
                  .sum
              )
            case AverageRatingOfOpponents      => averageRatingOfOpponentsCutN(0, allMyOpponentsGames)
            case AverageRatingOfOpponentsCut1  => averageRatingOfOpponentsCutN(1, allMyOpponentsGames)
            case AveragePerformanceOfOpponents =>
              val perfs = allMyOpponentsGames
                .map(opp => tb(TournamentPerformanceRating, opp.player, allPlayers))
              // FIDE says that the performance rating should be rounded up.
              average(perfs.sum, perfs.size.toFloat).map(_.round)
            case KoyaSystem =>
              val halfOfMaxPossibleScore = (allPlayers
                .map(_.games.size)
                .max) / 2f
              TieBreakPoints(
                myGames
                  .filter: game =>
                    allMyOpponentsGames
                      .exists(opponent =>
                        game.opponent == opponent.player && opponent.games.score >= halfOfMaxPossibleScore
                      )
                  .score
              )
            case SumOfProgressiveScores =>
              sumOfProgressiveScoresCutN(0, meAndMyGames)
            case SumOfProgressiveScoresCut1 =>
              sumOfProgressiveScoresCutN(1, meAndMyGames)
            case TournamentPerformanceRating =>
              TieBreakPoints(
                Elo
                  .computePerformanceRating(myGames.collect:
                    case POVGame(Some(points), Player(_, Some(rating)), _) => Elo.Game(points, rating))
                  .map(_.value.toFloat) | 0f
              )
            case PerfectTournamentPerformance =>
              val oppRatings = myGames.flatMap(_.opponent.rating.map(_.value))
              val minR       = oppRatings.min - 800
              val maxR       = oppRatings.max + 800
              if myGames.score == 0f && oppRatings.nonEmpty then TieBreakPoints(minR)
              else if oppRatings.isEmpty then TieBreakPoints(0f)
              else
                // Find the lowest integer rating R such that sum of expected scores >= myScore
                // Use the full FIDE conversion table, no ±400 cut
                def expectedScoreFor(r: Int): Float =
                  oppRatings
                    .map: oppR =>
                      Elo.getExpectedScore(oppR - r)
                    .sum
                @annotation.tailrec
                def binarySearch(low: Int, high: Int): Int =
                  if low >= high then low
                  else
                    val mid = (low + high) / 2
                    if expectedScoreFor(mid) >= myGames.score then binarySearch(low, mid)
                    else binarySearch(mid + 1, high)
                val ptp = binarySearch(minR, maxR)
                TieBreakPoints(ptp)

  case class POVGame(
      points: Option[chess.Outcome.Points],
      opponent: Player,
      color: Color
  )

  case class PlayerGames(
      player: Player,
      games: Seq[POVGame],
      partialTiebreaks: Option[NonEmptySeq[TieBreakPoints]] = None
  )

  case class Player(name: String, rating: Option[Elo])
