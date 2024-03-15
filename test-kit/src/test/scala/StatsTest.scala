package chess

class StatsTest extends ChessTest:

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float =
    val mean = realMean(elts).toDouble
    elts
      .map: x =>
        Math.pow(x - mean, 2)
      .sum
      .toFloat / (elts.size - 1)

  def beApprox(f: Float, comp: Float)(using munit.Location) =
    if comp.isNaN then assert(f.isNaN)
    else assertCloseTo(f, comp, 0.001f * comp)

  def beLike(comp: Stats) =
    (s: Stats) =>
      assertEquals(s.samples, comp.samples)
      beApprox(s.mean, comp.mean)
      (s.variance, comp.variance) match
        case (Some(sv), Some(cv)) => beApprox(sv, cv)
        case (sv, cv)             => assertEquals(sv, cv)

  test("empty stats: have good defaults"):
    assertEquals(Stats.empty.variance, None)
    assertEquals(Stats.empty.mean, 0f)
    assertEquals(Stats.empty.samples, 0)

  test("empty stats: make Stats"):

    assertEquals(Stats(5).samples, 1)
    assertEquals(Stats(5).variance, None)
    assertEquals(Stats(5).mean, 5f)

  test("large values"):
    // Tight data w/ large mean. Shuffled for Stats.
    val base         = (1 to 100) ++ (1 to 100) ++ (1 to 200)
    val data         = base.map { _ + 1e5f }
    val shuffledData = base.sortWith(_ % 8 > _ % 8).map { _ + 1e5f }

    val statsN = Stats.empty.record(shuffledData)
    beApprox(statsN.mean, realMean(data))
    beApprox(statsN.variance.get, realVar(data))
    assertEquals(statsN.samples, 400)
