package chess

import scalalib.time.{ nowSeconds, toSeconds }

import java.time.Instant

// times are expressed in seconds
case class CorrespondenceClock(
    base: Int,
    increment: Int,
    whiteTime: Float,
    blackTime: Float
):
  import CorrespondenceClock.*

  def daysPerTurn: Int = increment / 60 / 60 / 24

  def remainingTime(c: Color): Float = c.fold(whiteTime, blackTime)

  def outoftime(c: Color): Boolean = remainingTime(c) == 0

  def moretimeable(c: Color): Boolean = remainingTime(c) < (increment - hourSeconds)

  def giveTime(c: Color): CorrespondenceClock =
    c.fold(
      copy(whiteTime = whiteTime + daySeconds),
      copy(blackTime = blackTime + daySeconds)
    )

  // in seconds
  def estimateTotalTime: Int = base + increment * 40 / 2

  def incrementHours: Int = increment / 60 / 60

object CorrespondenceClock:
  val hourSeconds = 60 * 60
  val daySeconds  = 24 * hourSeconds

  // TODO: specify which color to add days to, if allowing addition to either color
  def apply(
      base: Int,
      increment: Int,
      remainingTime: Float,
      daysToAdd: Int,
      turnColor: Color,
      lastMoveAt: Instant
  ): CorrespondenceClock =
    val limit       = base
    val incremented = (remainingTime + (daysToAdd * daySeconds)).max(limit * daySeconds).toInt.max(0)
    val secondsLeft = (lastMoveAt.toSeconds + increment - nowSeconds).toInt.max(0)
    CorrespondenceClock(
      base = base,
      increment = increment,
      whiteTime = turnColor.fold(secondsLeft, incremented).toFloat,
      blackTime = turnColor.fold(incremented, secondsLeft).toFloat
    )
