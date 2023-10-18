package chess

class DecayingStatsTest extends ChessTest:

  import chess.DecayingStats as DS

  val random = java.util.Random(2286825201242408115L)

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float =
    val mean = realMean(elts).toDouble
    elts
      .map: x =>
        Math.pow(x - mean, 2)
      .sum
      .toFloat / (elts.size - 1)

  val randoms: Array[Float] = Array.fill(1000) { random.nextGaussian.toFloat }
  val data10: Array[Float]  = randoms map { _ + 10 }

  val stats10  = DS(10, 100, .9f).record(data10)
  val stats10d = DS(10, 100, 0.99f).record(data10)

  test("gaussian data: eventually converge with constant mean"):
    assertCloseTo(stats10.deviation, 1f, 0.25f)
    assertCloseTo(stats10.mean, 10f, 0.25f)

    assertCloseTo(stats10d.deviation, 1f, 0.25f)
    assertCloseTo(stats10d.mean, 10f, 0.1f)

  test("gaussian data: eventually converge with second mean"):
    val stats2  = stats10.record(randoms)
    val stats2d = stats10d.record(randoms)

    assertCloseTo(stats2.deviation, 1f, 0.25f)
    assertCloseTo(stats2.mean, 0f, 0.25f)

    assertCloseTo(stats2d.deviation, 1f, 0.25f)
    assertCloseTo(stats2d.mean, 0f, 0.1f)

  test("gaussian data: quickly converge with new mean"):
    assertCloseTo(stats10.record(randoms.take(20)).mean, 0f, 1f)
    // Not so quick with high decay...
    assertCloseTo(stats10d.record(randoms.take(100)).mean, 0f, 4f)

  test("gaussian data: converge with interleave"):
    val dataI   = Array(data10, randoms).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1)
    val statsIa = DS(10, 100, .9f).record(dataI)
    val statsIb = DS(10, 100, 0.99f).record(dataI)

    assertCloseTo(statsIa.deviation, 5f, 1f)
    assertCloseTo(statsIa.mean, 5f, 1f)

    assertCloseTo(statsIb.deviation, 5f, 0.25f)
    assertCloseTo(statsIb.mean, 5f, 0.25f)

  test("flip flop data should converge reasonably"):
    val data  = Array.iterate(0f, 1000) { 1f - _ }
    val stats = DS(0, 10, .9f).record(data)
    assertCloseTo(stats.mean, .5f, 0.05f)
    assertCloseTo(stats.deviation, .5f, 0.05f)
