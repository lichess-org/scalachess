package chess
package bitboard

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean

import Arbitraries.given
import Bitboard.*

class OpaqueBitboardTest extends ScalaCheckSuite:

  test("the result of addSquare should contain the added pos") {
    forAll { (bb: Bitboard, pos: Pos) =>
      assertEquals((bb.addSquare(pos).contains(pos)), true)
    }
  }

  test("pos.bb.singleSquare == Some(pos)") {
    forAll { (pos: Pos) =>
      assertEquals(pos.bb.singleSquare, Some(pos))
    }
  }

  test("count has the same value as squares.size") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.count, bb.squares.size)
    }
  }

  test("moreThanOne should be true when count > 1") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.moreThanOne, bb.count > 1)
    }
  }

  test("if moreThanOne then first is defined") {
    forAll { (bb: Bitboard) =>
      bb.moreThanOne ==> bb.first.isDefined
    }
  }

  test("singleSquare should be defined when count == 1") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.singleSquare.isDefined, bb.count == 1)
    }
  }

  test("fold removeFirst should return empty") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.fold(bb)((b, _) => b.removeFirst), Bitboard.empty)
    }
  }

  test("first with a function that always return None should return None") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.first(_ => None), None)
    }
  }

  test("first with a function that always return Some should return the first pos") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.first(Some(_)), bb.first)
    }
  }

  test("first returns the correct pos") {
    forAll { (xs: Set[Pos], x: Pos) =>
      def f = (p: Pos) => if p == x then Some(p) else None
      Bitboard(xs + x).first(f) == Some(x)
    }
  }
  test("bitboard created by set of pos should contains and only contains those pos") {
    forAll { (xs: Set[Pos]) =>
      val bb = Bitboard(xs)
      assertEquals(xs.forall(bb.contains), true)
      assertEquals(xs.size, bb.count)
    }
  }

  test("apply set of pos should be the same as using addSquare") {
    forAll { (xs: Set[Pos]) =>
      val bb  = Bitboard(xs)
      val bb2 = xs.foldLeft(Bitboard.empty)(_ addSquare _)
      assertEquals(bb, bb2)
    }
  }

  test("apply(Set[Pos]).squares.toSet == Set[Pos]") {
    forAll { (xs: Set[Pos]) =>
      val result = Bitboard(xs).squares.toSet
      assertEquals(result, xs)
    }
  }

  test("nonEmpty bitboard should have at least one square") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.nonEmpty, bb.first.isDefined)
    }
  }

  test("first should be the minimum of squares") {
    forAll { (bb: Bitboard) =>
      assertEquals(bb.first, bb.squares.minByOption(_.value))
    }
  }

  test("intersects should be true when the two bitboards have at least one common square") {
    forAll { (b1: Bitboard, b2: Bitboard, p: Pos) =>
      b1.addSquare(p).intersects(b2.addSquare(p))
    }
  }

  test("intersects and set intersection should be consistent") {
    forAll { (s1: Set[Pos], s2: Set[Pos]) =>
      val b1 = Bitboard(s1)
      val b2 = Bitboard(s2)
      val s  = s1 intersect s2
      b1.intersects(b2) == s.nonEmpty
    }
  }

  test("isDisjoint should be false when the two bitboards have at least common square") {
    forAll { (b1: Bitboard, b2: Bitboard, p: Pos) =>
      !b1.addSquare(p).isDisjoint(b2.addSquare(p))
    }
  }

  test("isDisjoint and intersects always return the opposite value") {
    forAll { (b1: Bitboard, b2: Bitboard) =>
      b1.isDisjoint(b2) == !b1.intersects(b2)
    }
  }
