package chess
package bitboard

import munit.ScalaCheckSuite
import org.scalacheck.Prop.{ forAll, propBoolean }

import CoreArbitraries.given

class BitboardTest extends ScalaCheckSuite:

  test("the result of add should contain the added square"):
    forAll: (bb: Bitboard, square: Square) =>
      assertEquals(bb.add(square).contains(square), true)

  test("contains by square always consistent with contains by file and rank"):
    forAll: (bb: Bitboard, square: Square) =>
      assertEquals(bb.contains(square), bb.contains(square.file, square.rank))

  test("supersetOf and subsetOf"):
    forAll: (b1: Bitboard, b2: Bitboard) =>
      assert((b1 | b2).supersetOf(b1))
      assert((b1 | b2).supersetOf(b2))
      assert((b1 & b2).subsetOf(b1))
      assert((b1 & b2).subsetOf(b2))
      assert(Bitboard.all.supersetOf(b1))
      assert(Bitboard.all.supersetOf(b2))
      assert(Bitboard.empty.subsetOf(b1))
      assert(Bitboard.empty.subsetOf(b2))

  test("square.bb.singleSquare == Some(square)"):
    forAll: (square: Square) =>
      assertEquals(square.bb.singleSquare, Some(square))

  test("count has the same value as squares.size"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.count, bb.squares.size)

  test("moreThanOne should be true when count > 1"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.moreThanOne, bb.count > 1)

  test("if moreThanOne then first is defined"):
    forAll: (bb: Bitboard) =>
      bb.moreThanOne ==> bb.first.isDefined

  test("singleSquare should be defined when count == 1"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.singleSquare.isDefined, bb.count == 1)

  test("singleSquare <=> first == last"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.singleSquare.isDefined, bb.first == bb.last)

  property("fold removeFirst should return empty"):
    forAll: (bb: Bitboard) =>
      bb.fold(bb)((b, _) => b.removeFirst).isEmpty

  property("fold removeFirst.add(first) == identity"):
    forAll: (bb: Bitboard) =>
      bb.nonEmpty ==> {
        bb.removeFirst.add(bb.first.get) == bb
      }

  property("bb.removeFirst == bb <=> bb.isEmpty"):
    forAll: (bb: Bitboard) =>
      if bb.isEmpty then bb.removeFirst == bb
      else bb.removeFirst != bb

  test("fold removeFirst should return empty"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.fold(bb)((b, _) => b.removeFirst), Bitboard.empty)

  property("fold removeLast should return empty"):
    forAll: (bb: Bitboard) =>
      bb.fold(bb)((b, _) => b.removeLast).isEmpty

  property("fold removeLast.add(last) == identity"):
    forAll: (bb: Bitboard) =>
      bb.nonEmpty ==> {
        bb.removeLast.add(bb.last.get) == bb
      }

  property("bb.removeLast == bb <=> bb.isEmpty"):
    forAll: (bb: Bitboard) =>
      if bb.isEmpty then bb.removeLast == bb
      else bb.removeLast != bb

  property("removeFirst == removeLast <=> bb.count <= 1"):
    forAll: (bb: Bitboard) =>
      bb.removeFirst == bb.removeLast == (bb.count <= 1)

  test("first with a function that always return None should return None"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.first(_ => None), None)

  test("first with a function that always return Some should return the first square"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.first(Some(_)), bb.first)

  test("first returns the correct square"):
    forAll: (xs: Set[Square], x: Square) =>
      def f = (p: Square) => if p == x then Some(p) else None
      Bitboard(xs + x).first(f) == Some(x)

  test("last with a function that always return None should return None"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.last(_ => None), None)

  test("last with a function that always return Some should return the last square"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.last(Some(_)), bb.last)

  test("last returns the correct square"):
    forAll: (xs: Set[Square], x: Square) =>
      def f = (p: Square) => if p == x then Some(p) else None
      Bitboard(xs + x).last(f) == Some(x)

  test("find"):
    forAll: (bb: Bitboard, f: Square => Boolean) =>
      bb.find(f) == bb.squares.find(f)

  test("findLast"):
    forAll: (bb: Bitboard, f: Square => Boolean) =>
      bb.findLast(f) == bb.squares.reverse.find(f)

  test("bitboard created by set of square should contains and only contains those square"):
    forAll: (xs: Set[Square]) =>
      val bb = Bitboard(xs)
      assertEquals(xs.forall(bb.contains), true)
      assertEquals(xs.size, bb.count)

  test("apply list of square should be the same as using add"):
    forAll: (xs: List[Square]) =>
      val bb = Bitboard(xs)
      val bb2 = xs.foldLeft(Bitboard.empty)(_.add(_))
      assertEquals(bb, bb2)

  test("apply varargs of square should be the same as apply with list"):
    forAll: (xs: List[Square]) =>
      val bb = Bitboard(xs)
      val bb2 = Bitboard(xs*)
      assertEquals(bb, bb2)

  test("add andThen remove should be the same as identity"):
    forAll: (bb: Bitboard, square: Square) =>
      !bb.contains(square) ==> assertEquals(bb.add(square).remove(square), bb)

  test("move == add. remove"):
    forAll: (bb: Bitboard, from: Square, to: Square) =>
      from != to ==> {
        bb.move(from, to) == bb.add(to).remove(from)
      }

  test("apply(Set[square]).squares.toSet == Set[square]"):
    forAll: (xs: Set[Square]) =>
      val result = Bitboard(xs).squares.toSet
      assertEquals(result, xs)

  test("Set >>= Bitboard >>= toSet === identity"):
    forAll: (xs: Set[Square]) =>
      assertEquals(Bitboard(xs).toSet, xs)

  test("nonEmpty bitboard should have at least one square"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.nonEmpty, bb.first.isDefined)
      assertEquals(bb.nonEmpty, bb.last.isDefined)

  test("two bits bitboard should have first and last defined"):
    forAll: (s1: Square, s2: Square) =>
      s1 != s2 ==> {
        val bb = s1.bb | s2.bb
        bb.first.isDefined &&
        bb.last.isDefined &&
        bb.first != bb.last &&
        List(bb.first, bb.last).flatten.toSet == Set(s1, s2)
      }

  test("first should be the minimum of squares"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.first, bb.squares.minByOption(_.value))

  test("last should be the max of squares"):
    forAll: (bb: Bitboard) =>
      assertEquals(bb.last, bb.squares.maxByOption(_.value))

  test("intersects should be true when the two bitboards have at least one common square"):
    forAll: (b1: Bitboard, b2: Bitboard, p: Square) =>
      b1.add(p).intersects(b2.add(p))

  test("intersects and set intersection should be consistent"):
    forAll: (s1: Set[Square], s2: Set[Square]) =>
      val b1 = Bitboard(s1)
      val b2 = Bitboard(s2)
      val s = s1.intersect(s2)
      b1.intersects(b2) == s.nonEmpty

  test("isDisjoint should be false when the two bitboards have at least common square"):
    forAll: (b1: Bitboard, b2: Bitboard, p: Square) =>
      !b1.add(p).isDisjoint(b2.add(p))

  test("isDisjoint and intersects always return the opposite value"):
    forAll: (b1: Bitboard, b2: Bitboard) =>
      b1.isDisjoint(b2) != b1.intersects(b2)

  property("forall"):
    forAll: (b: Bitboard, f: Square => Boolean) =>
      b.forall(f) == b.squares.forall(f)

  property("exists"):
    forAll: (b: Bitboard, f: Square => Boolean) =>
      b.exists(f) == b.squares.exists(f)

  property("map"):
    forAll: (b: Bitboard, f: Square => Int) =>
      b.map(f) == b.squares.map(f)

  property("flatMap"):
    forAll: (b: Bitboard, f: Square => Option[Int]) =>
      b.flatMap(f) == b.squares.flatMap(f)

  property("filter"):
    forAll: (b: Bitboard, f: Square => Boolean) =>
      b.filter(f) == b.squares.filter(f)

  property("first"):
    forAll: (b: Bitboard, f: Square => Option[Int]) =>
      b.first(f) == b.squares.map(f).find(_.isDefined).flatten

  property("last"):
    forAll: (b: Bitboard, f: Square => Option[Int]) =>
      b.last(f) == b.squares.map(f).findLast(_.isDefined).flatten

  property("foreach"):
    forAll: (b: Bitboard, f: Square => Int) =>
      var s1 = 0L
      var s2 = 0L
      b.foreach(x => s1 += f(x).toLong)
      b.squares.foreach(x => s2 += f(x).toLong)
      s1 == s2
