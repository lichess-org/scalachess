package chess

import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

class DecayingStatsTest extends Specification with ValidationMatchers {

  def realMean(elts: Seq[Float]): Float = elts.sum / elts.size

  def realVar(elts: Seq[Float]): Float = {
    val mean = realMean(elts).toDouble
    (elts map { x => Math.pow(x - mean, 2) } sum).toFloat / (elts.size - 1)
  }

  def beApprox(comp: Float) = (f: Float) => {
    comp must beCloseTo(f +/- 0.001f * comp)
  }

  // TODO
}
