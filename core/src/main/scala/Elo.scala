package chess

opaque type Elo = Int

opaque type KFactor = Int
object KFactor extends OpaqueInt[KFactor]

object Elo extends OpaqueInt[Elo]:

  trait Player:
    def rating: Elo
    def kFactor: KFactor

  trait Game:
    def win: Option[Boolean]
    def opponentRating: Elo

  // https://en.wikipedia.org/wiki/Elo_rating_system#Mathematical_details
  def computeNewEloRating(player: Player, games: Seq[Game]): Elo =
    val expectedScore = games.foldLeft(0d): (score, game) =>
      score + 1 / (1 + Math.pow(10, (game.opponentRating - player.rating).value / 400))
    val achievedScore = games.foldLeft(0d): (score, game) =>
      score + game.win.fold(0.5d)(if _ then 1d else 0d)
    val ratingDiff = Math.round(player.kFactor * (achievedScore - expectedScore)).toInt
    player.rating + ratingDiff
