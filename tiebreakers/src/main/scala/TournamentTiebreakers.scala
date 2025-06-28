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
| Perfect Tournament Performance                      | DB   | 10.3    | PTP     |       | ❌
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

opaque type TieBreakPoints = Float
object TieBreakPoints extends OpaqueFloat[TieBreakPoints]
given Numeric[TieBreakPoints] = Numeric[Float]

extension (tieBreakSeq: Seq[TieBreakPoints])
  def cutSum(cut: Int): TieBreakPoints =
    tieBreakSeq.sorted.drop(cut).sum

object Tiebreaker:

  val byCode: Map[String, Tiebreaker] = values.mapBy(_.code)

  private def buchholzCutN(cut: Int, opponentGames: Seq[PlayerGames]): TieBreakPoints =
    opponentGames
      .map: opponent =>
        TieBreakPoints(opponent.score)
      .cutSum(cut)

  private def sonnebornBergerCutN(
      cut: Int,
      player: PlayerGames,
      opponentGames: Seq[PlayerGames]
  ): TieBreakPoints =
    player.games
      .map: game =>
        val oppScore = opponentGames.find(_.player == game.opponent).map(_.score)
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
        .map(elo => TieBreakPoints(elo.player.rating.value))
        .cutSum(cut),
      opponentGames.size.toFloat
    ).map(_.round) // Must round up according to FIDE rules

  private def sumOfProgressiveScoresCutN(
      cut: Int,
      player: PlayerGames
  ): TieBreakPoints =
    player.games.indices
      .map: i =>
        TieBreakPoints(player.copy(games = player.games.take(i + 1)).score)
      .cutSum(cut)

  def tb(tiebreaker: Tiebreaker, me: Player, allPlayers: Seq[PlayerGames]): TieBreakPoints =
    val myGames      = allPlayers.find(_.player == me)
    val allOpponents = allPlayers.filter(_.games.exists(_.opponent == me))
    myGames.fold(TieBreakPoints(0f)): meAndMyGames =>
      meAndMyGames match
        case PlayerGames(_, myGames, partialTiebreaks) =>
          tiebreaker match
            case NbBlackGames => TieBreakPoints(myGames.filter(_.color == Color.Black).size)
            case NbWins       => TieBreakPoints(myGames.count(_.points.contains(Points.One)))
            case NbBlackWins  =>
              TieBreakPoints(
                myGames.count(g => g.color == Color.Black && g.points.contains(Points.One))
              )
            case SonnebornBerger            => sonnebornBergerCutN(0, meAndMyGames, allOpponents)
            case SonnebornBergerCut1        => sonnebornBergerCutN(1, meAndMyGames, allOpponents)
            case Buchholz                   => buchholzCutN(0, allOpponents)
            case BuchholzCut1               => buchholzCutN(1, allOpponents)
            case BuchholzCut2               => buchholzCutN(2, allOpponents)
            case AverageOfOpponentsBuchholz =>
              average(
                allOpponents
                  .map: opp =>
                    tb(Buchholz, opp.player, allPlayers)
                  .sum,
                allOpponents.size.toFloat
              )
            case DirectEncounter =>
              TieBreakPoints(
                myGames
                  .groupBy(_.opponent)
                  .map: (opponent, games) =>
                    val opponentGames        = allOpponents.find(_.player == opponent)
                    val validDirectEncounter =
                      opponentGames.exists(_.score == meAndMyGames.score) && partialTiebreaks
                        .zip(opponentGames.flatMap(_.partialTiebreaks))
                        .forall(_ == _)
                    lazy val scoreAgainstOpp = games.flatMap(_.points.map(_.value)).sum
                    if !validDirectEncounter then 0f
                    // If the players meet more than once, FIDE dictates that we average the score
                    else if games.size > 1 then scoreAgainstOpp / games.size.toFloat
                    else scoreAgainstOpp
                  .sum
              )
            case AverageRatingOfOpponents      => averageRatingOfOpponentsCutN(0, allOpponents)
            case AverageRatingOfOpponentsCut1  => averageRatingOfOpponentsCutN(1, allOpponents)
            case AveragePerformanceOfOpponents =>
              val perfs = allOpponents
                .map(opp => tb(TournamentPerformanceRating, opp.player, allPlayers))
              // FIDE says that the performance rating should be rounded up.
              average(perfs.sum, perfs.size.toFloat).map(_.round)
            case KoyaSystem =>
              val halfOfMaxPossibleScore = (allPlayers
                .map(_.games.size)
                .max) / 2f
              TieBreakPoints(
                meAndMyGames
                  .copy(games = myGames.filter: game =>
                    allOpponents
                      .exists(opponent =>
                        game.opponent == opponent.player && opponent.score >= halfOfMaxPossibleScore
                      ))
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
                    case POVGame(Some(points), opponent, _) => Elo.Game(points, opponent.rating))
                  .map(_.value.toFloat) | 0f
              )

  case class POVGame(
      points: Option[chess.Outcome.Points],
      opponent: Player,
      color: Color
  )

  case class PlayerGames(
      player: Player,
      games: Seq[POVGame],
      partialTiebreaks: Option[NonEmptySeq[TieBreakPoints]] = None
  ):
    def score: Float = games.flatMap(_.points.map(_.value)).sum

  case class Player(name: String, rating: Elo)
