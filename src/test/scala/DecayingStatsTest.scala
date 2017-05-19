package chess

import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

import scala.collection.breakOut

import DecayingStats.{ empty => dEmpty }

class DecayingStatsTest extends Specification with ValidationMatchers {

  val random = new java.util.Random(2286825201242408115l)

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float = {
    val mean = realMean(elts).toDouble
    (elts map { x => Math.pow(x - mean, 2) } sum).toFloat / (elts.size - 1)
  }

  def beApprox(comp: Float) = (f: Float) => {
    comp must beCloseTo(f +/- 0.1f * comp)
  }

  "empty stats" should {
    "propogate initial state" in {
      dEmpty(0.5f).record(0).mean must_== 0
      dEmpty(0.5f).record(0).variance must_== 0.5f
      dEmpty(0, decay = 0.9f).record(0).decay must_== 0.9f
    }
  }

  "gaussian data" should {
    val randoms = Array.fill(1000) { random.nextGaussian.toFloat }
    val data10 = randoms map { _ + 10 }

    val stats10 = dEmpty(100).record(10).record(data10)
    val stats10d = dEmpty(100, decay = 0.99f).record(10).record(data10)
    "eventually converge with constant mean" in {
      stats10.stdDev must beCloseTo(1f +/- 0.2f)
      stats10.mean must beCloseTo(10f +/- 0.25f)

      stats10d.stdDev must beCloseTo(1f +/- 0.005f)
      stats10d.mean must beCloseTo(10f +/- 0.1f)
    }

    "eventually converge with second mean" in {
      val stats2 = stats10.record(randoms)
      val stats2d = stats10d.record(randoms)

      stats2.stdDev must beCloseTo(1f +/- 0.2f)
      stats2.mean must beCloseTo(0f +/- 0.25f)

      stats2d.stdDev must beCloseTo(1f +/- 0.005f)
      stats2d.mean must beCloseTo(0f +/- 0.1f)
    }

    "quickly converge with new mean" in {
      stats10.record(randoms.take(20)).mean must beCloseTo(0f +/- 1f)
      // Not so quick with high decay...
      stats10d.record(randoms.take(100)).mean must beCloseTo(0f +/- 4f)
    }

    "converge with interleave" in {
      val dataI = Array(data10, randoms).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1)
      val statsIa = dEmpty(100).record(10).record(dataI)
      val statsIb = dEmpty(100, decay = 0.99f).record(10).record(dataI)

      statsIa.stdDev must beCloseTo(5f +/- 1f)
      statsIa.mean must beCloseTo(5f +/- 1f)

      statsIb.stdDev must beCloseTo(5f +/- 0.25f)
      statsIb.mean must beCloseTo(5f +/- 0.25f)
    }
  }

  "flip flop data" should {
    val data: Array[Float] = (1 to 1000).map { i => (i & 1).toFloat }(breakOut)
    val stats = dEmpty(10).record(0).record(data)
    "converge reasonably" in {
      stats.mean must beCloseTo(.5f +/- 0.05f)
      stats.stdDev must beCloseTo(.5f +/- 0.05f)
    }
  }
}
