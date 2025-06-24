package chess

import chess.Outcome.Points
import scalalib.newtypes.*

opaque type TieBreakPoints = Float
object TieBreakPoints extends OpaqueFloat[TieBreakPoints]

extension (tieBreakSeq: Seq[TieBreakPoints])
  def cutSum(cut: Int): TieBreakPoints =
    tieBreakSeq.sorted.drop(cut).sum

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

  // case AverageOfOpponentsBuchholz extends Tiebreaker("AOB", "Average of opponents Buchholz score")

  case DirectEncounter extends Tiebreaker("DE", "Direct encounter")

  case AverageOpponentRating extends Tiebreaker("AOR", "Average opponent rating")

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

  def tb(tiebreaker: Tiebreaker, player: PlayerGames, opponentGames: Seq[PlayerGames]): TieBreakPoints =
    tiebreaker match
      case NbGames      => TieBreakPoints(player.games.size)
      case NbBlackGames => TieBreakPoints(player.games.filter(_.color == Color.Black).size)
      case NbWins       => TieBreakPoints(player.games.count(_.points.contains(Points.One)))
      case NbBlackWins =>
        TieBreakPoints(player.games.count(g => g.color == Color.Black && g.points.contains(Points.One)))
      case SonnebornBerger     => SonnebornBergerCutN(0, player, opponentGames)
      case SonnebornBergerCut1 => SonnebornBergerCutN(1, player, opponentGames)
      case Buchholz            => BuchholzCutN(0, opponentGames)
      case BuchholzCut1        => BuchholzCutN(1, opponentGames)
      case BuchholzCut2        => BuchholzCutN(2, opponentGames)

      case DirectEncounter =>
        TieBreakPoints(
          player
            .copy(games = player.games.filter: game =>
              opponentGames
                .exists(opponent => opponent.player == game.opponent && opponent.score == player.score))
            .score
        )
      case AverageOpponentRating =>
        TieBreakPoints(
          opponentGames
            .map(_.player.rating)
            .sum / opponentGames.size.toFloat
        )

case class POVGame(
    points: Option[chess.Outcome.Points],
    opponent: Player,
    color: Color
)

case class PlayerGames(player: Player, games: Seq[POVGame]):
  def score: Float = games.flatMap(_.points.map(_.value)).sum

case class Player(name: String, rating: Int)
