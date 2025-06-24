package chess.tiebreakers

import chess.rating.Elo
import chess.Outcome.Points
import chess.Color
import scalalib.newtypes.*
import scalalib.extensions.*

opaque type TieBreakPoints = Float
object TieBreakPoints extends OpaqueFloat[TieBreakPoints]

extension (tieBreakSeq: Seq[TieBreakPoints])
  def cutSum(cut: Int): TieBreakPoints =
    tieBreakSeq.sorted.drop(cut).sum

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

  case AveragePerformanceOfOpponents
      extends Tiebreaker(
        "APRO",
        "Average performance of opponents"
      )

  case KoyaSystem extends Tiebreaker("KS", "Koya system")

  case SumOfProgressiveScores extends Tiebreaker("PS", "Sum of progressive scores")

  case SumOfProgressiveScoresCut1 extends Tiebreaker("PS-C1", "Sum of progressive scores cut 1")

object Tiebreaker:

  private def BuchholzCutN(cut: Int, opponentGames: Seq[PlayerGames]): TieBreakPoints =
    opponentGames
      .map: opponent =>
        TieBreakPoints(opponent.score)
      .cutSum(cut)

  private def SonnebornBergerCutN(
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

  private def AverageRatingOfOpponentsCutN(
      cut: Int,
      opponentGames: Seq[PlayerGames]
  ): TieBreakPoints =
    TieBreakPoints(
      opponentGames
        .map(_.player.rating.value)
        .sorted
        .drop(cut)
        .sum / opponentGames.size.toFloat
    )

  private def SumOfProgressiveScoresCutN(
      cut: Int,
      player: PlayerGames
  ): TieBreakPoints =
    val progressiveScores = player.games.indices
      .map: i =>
        player.copy(games = player.games.take(i + 1))
      .map(_.score)
    TieBreakPoints(progressiveScores.sorted.drop(cut).sum)

  def tb(tiebreaker: Tiebreaker, player: Player, allGames: Seq[PlayerGames]): TieBreakPoints =
    val playerGamesOpt = allGames.find(_.player == player)
    val opponentGames  = allGames.filter(_.games.exists(_.opponent == player))
    playerGamesOpt.fold(TieBreakPoints(0f)): playerWithGames =>
      playerWithGames match
        case PlayerGames(player, games) =>
          tiebreaker match
            case NbBlackGames => TieBreakPoints(games.filter(_.color == Color.Black).size)
            case NbWins       => TieBreakPoints(games.count(_.points.contains(Points.One)))
            case NbBlackWins =>
              TieBreakPoints(
                games.count(g => g.color == Color.Black && g.points.contains(Points.One))
              )
            case SonnebornBerger     => SonnebornBergerCutN(0, playerWithGames, opponentGames)
            case SonnebornBergerCut1 => SonnebornBergerCutN(1, playerWithGames, opponentGames)
            case Buchholz            => BuchholzCutN(0, opponentGames)
            case BuchholzCut1        => BuchholzCutN(1, opponentGames)
            case BuchholzCut2        => BuchholzCutN(2, opponentGames)
            case AverageOfOpponentsBuchholz =>
              TieBreakPoints(
                opponentGames
                  .map: opp =>
                    tb(Buchholz, opp.player, allGames).value
                  .sum / opponentGames.size.toFloat
              )
            case DirectEncounter =>
              TieBreakPoints(
                playerWithGames
                  .copy(games = games.filter: game =>
                    opponentGames
                      .exists(opponent =>
                        opponent.player == game.opponent && opponent.score == playerWithGames.score
                      ))
                  .score
              )
            case AverageRatingOfOpponents     => AverageRatingOfOpponentsCutN(0, opponentGames)
            case AverageRatingOfOpponentsCut1 => AverageRatingOfOpponentsCutN(1, opponentGames)
            case AveragePerformanceOfOpponents =>
              val perfs = opponentGames
                .map: opp =>
                  Elo
                    .computePerformanceRating(opp.games.collect:
                      case POVGame(Some(points), _, _) => Elo.Game(points, opp.player.rating))
                    .map(_.value)
                .flatten
              TieBreakPoints(perfs.nonEmpty.option(perfs.sum / perfs.size.toFloat) | 0f)
            case KoyaSystem =>
              val halfOfMaxPossibleScore = allGames
                .map(_.games.size)
                .max / 2
              tb(Buchholz, player, allGames.filter(_.score >= halfOfMaxPossibleScore))
            case SumOfProgressiveScores =>
              SumOfProgressiveScoresCutN(0, playerWithGames)
            case SumOfProgressiveScoresCut1 =>
              SumOfProgressiveScoresCutN(1, playerWithGames)

  case class POVGame(
      points: Option[chess.Outcome.Points],
      opponent: Player,
      color: Color
  )

  case class PlayerGames(player: Player, games: Seq[POVGame]):
    def score: Float = games.flatMap(_.points.map(_.value)).sum

  case class Player(name: String, rating: Elo)
