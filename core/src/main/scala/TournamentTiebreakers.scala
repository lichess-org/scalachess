package chess

import chess.Outcome.Points

// https://handbook.fide.com/chapter/TieBreakRegulations082024
enum Tiebreaker(val code: String, val name: String):

  case NbGames extends Tiebreaker("GAMES", "Number of games played")

  case NbBlackGames extends Tiebreaker("BPG", "Number of games played with black")

  case NbWins extends Tiebreaker("WIN", "Number of Wins")

  case NbBlackWins extends Tiebreaker("BWG", "Number of wins with black")

  case NbWinsExcludingByes extends Tiebreaker("WON", "Number of wins excluding byes")

  case SonnebornBerger extends Tiebreaker("SB", "Sonneborn-Berger score")

  case Buchholz extends Tiebreaker("BH", "Buchholz score")

  case BuchholzCut1 extends Tiebreaker("BH-C1", "Buchholz cut 1")

  case BuchholzCut2 extends Tiebreaker("BH-C2", "Buchholz cut 2")

  case DirectEncounter extends Tiebreaker("DE", "Direct encounter")

object Tiebreaker:

  private def BuccholzCutN(n: Int, opponents: Seq[PlayerGames]): Float =
    opponents.map(_.score).sorted.drop(n).sum

  def tb(tiebreaker: Tiebreaker, playerName: String, allGames: Seq[PlayerGames]): Float =
    allGames
      .find(_.name == playerName)
      .fold(0f): player =>
        tb(tiebreaker, player, allGames.filterNot(_.name == playerName))

  def tb(tiebreaker: Tiebreaker, player: PlayerGames, opponents: Seq[PlayerGames]): Float =
    tiebreaker match
      case NbGames => player.games.size.toFloat
      case NbBlackGames =>
        tb(NbGames, player.copy(games = player.games.filter(_.color == Color.Black)), opponents)
      case NbWins => player.games.count(_.points.contains(Points.One)).toFloat
      case NbBlackWins =>
        tb(NbWins, player.copy(games = player.games.filter(_.color == Color.Black)), opponents)
      case NbWinsExcludingByes => tb(NbWins, player.copy(games = player.games.filter(_.plies > 2)), opponents)
      case SonnebornBerger =>
        opponents
          .map: opponent =>
            player.games
              .find(_.opponent == opponent.name)
              .fold(0f)(
                _.points
                  .map:
                    case Points.Zero => 0f
                    case Points.Half => opponent.score / 2
                    case Points.One  => opponent.score
                  .sum
              )
          .sum
      case Buchholz     => BuccholzCutN(0, opponents)
      case BuchholzCut1 => BuccholzCutN(1, opponents)
      case BuchholzCut2 => BuccholzCutN(2, opponents)
      case DirectEncounter =>
        player
          .copy(games = player.games.filter: game =>
            opponents.exists(op => op.score == player.score && op.name == game.opponent))
          .score

case class POVGame(
    plies: Ply,
    points: Option[chess.Outcome.Points],
    opponent: String,
    color: Color
)

case class PlayerGames(name: String, games: Seq[POVGame]):
  def score: Float = games.flatMap(_.points.map(_.value)).sum
