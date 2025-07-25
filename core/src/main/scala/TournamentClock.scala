package chess

import cats.syntax.all.*

import Clock.{ LimitSeconds, LimitMinutes, IncrementSeconds }

case class TournamentClock(limitSeconds: LimitSeconds, incrementSeconds: IncrementSeconds):

  def limit: Centis = Centis.ofSeconds(limitSeconds.value)
  def increment: Centis = Centis.ofSeconds(incrementSeconds.value)

  def limitMinutes = LimitMinutes(limitSeconds.value / 60)

  def toClockConfig: Option[Clock.Config] = Clock.Config(limitSeconds, incrementSeconds).some

  override def toString = s"$limitMinutes+$incrementSeconds"

object TournamentClock:

  object parse:

    private val cleanRegex = "(/move|minutes|minute|min|m|seconds|second|sec|s|'|\")".r

    private def make(strict: Boolean)(a: Int, b: Int) =
      val limit = if strict then LimitSeconds(a) else LimitSeconds(if a > 180 then a else a * 60)
      TournamentClock(limit, IncrementSeconds(b))

    // `strict` uses PGN specification https://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6.1
    // where time control is always in seconds
    def apply(strict: Boolean)(str: String): Option[TournamentClock] =
      cleanRegex
        .replaceAllIn(str.toLowerCase.replace(" ", ""), "")
        .split('+')
        .match
          case Array(a) => a.toIntOption.map(make(strict)(_, 0))
          case Array(a, b) => (a.toIntOption, b.toIntOption).mapN(make(strict))
          case _ => none
