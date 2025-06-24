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

// Tie-breakers for individuals for swiss/round-robins
// https://handbook.fide.com/chapter/TieBreakRegulations082024
enum Tiebreaker(val code: String, val name: String):

  case NbGames extends Tiebreaker("GAMES", "Number of games played")

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

  case AverageOpponentRating extends Tiebreaker("AOR", "Average opponent rating")

  case AverageOpponentRatingCut1 extends Tiebreaker("AOR-C1", "Average opponent rating cut 1")

  case AveragePerformanceOfOpponents
      extends Tiebreaker(
        "APRO",
        "Average performance of opponents"
      )

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

  private def AverageOpponentRatingCutN(
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

  def tb(tiebreaker: Tiebreaker, player: Player, allGames: Seq[PlayerGames]): TieBreakPoints =
    val playerGamesOpt = allGames.find(_.player == player)
    val opponentGames  = allGames.filter(_.games.exists(_.opponent == player))
    playerGamesOpt.fold(TieBreakPoints(0f)): playerWithGames =>
      playerWithGames match
        case PlayerGames(player, games) =>
          tiebreaker match
            case NbGames      => TieBreakPoints(games.size)
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
            case AverageOpponentRating     => AverageOpponentRatingCutN(0, opponentGames)
            case AverageOpponentRatingCut1 => AverageOpponentRatingCutN(1, opponentGames)
            case AveragePerformanceOfOpponents =>
              val perfs = opponentGames
                .map: opp =>
                  Elo
                    .computePerformanceRating(opp.games.collect:
                      case POVGame(Some(points), _, _) => Elo.Game(points, opp.player.rating))
                    .map(_.value)
                .flatten
              TieBreakPoints(perfs.nonEmpty.option(perfs.sum / perfs.size.toFloat) | 0f)

  case class POVGame(
      points: Option[chess.Outcome.Points],
      opponent: Player,
      color: Color
  )

  case class PlayerGames(player: Player, games: Seq[POVGame]):
    def score: Float = games.flatMap(_.points.map(_.value)).sum

  case class Player(name: String, rating: Elo)
