package chess

import cats.syntax.all.*

import Clock.{ LimitSeconds, LimitMinutes, IncrementSeconds }

sealed trait TournamentClock:
  def limit: Centis
  def increment: Centis

  def incrementAtPly(ply: Ply): Centis

  def limitMinutes: LimitMinutes

  def toClockConfig: Option[Clock.Config]

case class GameStage(from: Int, to: Option[Int])

// support delay
object TournamentClock:
  case class Single(limitSeconds: LimitSeconds, incrementSeconds: IncrementSeconds) extends TournamentClock:
    def limit: Centis     = Centis.ofSeconds(limitSeconds.value)
    def increment: Centis = Centis.ofSeconds(incrementSeconds.value)

    def incrementAtPly(ply: Ply): Centis = increment

    def limitMinutes = LimitMinutes(limitSeconds.value / 60)

    def toClockConfig: Option[Clock.Config] = Clock.Config(limitSeconds, incrementSeconds).some

  case class Stages(stages: List[(GameStage, Single)]) extends TournamentClock:
    def limit: Centis     = ???
    def increment: Centis = ???

    def incrementAtPly(ply: Ply): Centis = ???

    def limitMinutes = ???

    def toClockConfig: Option[Clock.Config] = ???

  case class Armageddon(white: Single, black: Single) extends TournamentClock:
    def limit: Centis                       = ???
    def increment: Centis                   = ???
    def incrementAtPly(ply: Ply): Centis    = ???
    def limitMinutes                        = ???
    def toClockConfig: Option[Clock.Config] = ???

  object parse:

    private val cleanRegex = "(/move|minutes|minute|min|m|seconds|second|sec|s|'|\")".r

    private def make(strict: Boolean)(a: Int, b: Int): TournamentClock =
      val limit = if strict then LimitSeconds(a) else LimitSeconds(if a > 180 then a else a * 60)
      TournamentClock.Single(limit, IncrementSeconds(b))

    // `strict` uses PGN specification https://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6.1
    // where time control is always in seconds
    def apply(strict: Boolean)(str: String): Option[TournamentClock] =
      cleanRegex
        .replaceAllIn(str.toLowerCase.replace(" ", ""), "")
        .split('+')
        .match
          case Array(a)    => a.toIntOption.map(make(strict)(_, 0))
          case Array(a, b) => (a.toIntOption, b.toIntOption).mapN(make(strict))
          case _           => none
