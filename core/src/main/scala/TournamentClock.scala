package chess

import Clock.{ LimitSeconds, IncrementSeconds }

case class TournamentClock(limitSeconds: LimitSeconds, incrementSeconds: IncrementSeconds)

object TournamentClock:

  def parse(str: String): Option[TournamentClock] = None
