package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.{ *, given }
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import cats.kernel.Monoid

class NodeTest extends ScalaCheckSuite:

  given HasId[Int, Int] with
    def getId(a: Int): Int = a

  test("mainline size <= node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size <= node.size

  test("mainline is a sub set of node"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.forall(x => node.exists(_ == x))

  test("mainline.size + variations.size == node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size + variationsCount(node) == node.size

  test("find ids with mainline as path retuns last mainline node"):
    forAll: (node: Node[Int]) =>
      node.find(node.mainlineValues).get == node.lastMainlineNode

  test("use mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.findPath(node.mainlineValues) == node.mainline.some

  test("use subset of mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.size >= 2 ==> {
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.findPath(path).isDefined
      }

  test("use randomPath for findPath"):
    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      path.nonEmpty ==> {
        val found = node.findPath(path).map(_.map(_.value))
        found.isEmpty || (found.isDefined && found.get == path)
      }

  test("with 0 < n <= node.mainline.size => take(n).mainline size == n"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.take(n).mainline.size == n
      }

  test("findPath.take(n).mainlineValues isDefined"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.findPath(node.take(n).mainlineValues).isDefined
      }

  test("take(node.mainline.size) == node"):
    forAll: (node: Node[Int]) =>
      node.take(node.mainline.size) == node

  test("apply(n) return None if n >= node.mainline.size"):
    forAll: (node: Node[Int]) =>
      node(node.mainline.size).isEmpty

  test("apply(n) return None if n < node.size"):
    forAll: (node: Node[Int]) =>
      val n = if node.mainline.size == 1 then 0 else Random.nextInt(node.mainline.size - 1)
      node(n).isDefined

  test("apply(0) return itself"):
    forAll: (node: Node[Int]) =>
      node(0) == node.some

  test("take(n).size + apply(n).size == node.size"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.take(n).size + node(n).map(_.size).getOrElse(0L) == node.size
      }

  test("modifyAt with mainline == modifyLastMainlineNode"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node.modifyAt(node.mainlineValues, Tree.lift(f)) == node.modifyLastMainlineNode(Node.lift(f)).some

  test("modifyAt and find are consistent"):
    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      def f[A]: TreeMapper[A] = node =>
        node match
          case n: Node[A]      => n
          case v: Variation[A] => v
      node.modifyAt(path, f).flatMap(_.find(path)) == node.find(path)

  test("deleteAt with root value return Some(None)"):
    forAll: (node: Node[Int]) =>
      val deleted = node.deleteAt(List(node.value))
      deleted == Some(None)

  test("deleteAt with mainline == deleteLastMainlineNode"):
    forAll: (node: Node[Int]) =>
      node.size >= 2 ==> {
        val deleted = node.deleteAt(node.mainlineValues)
        deleted.isDefined && deleted.get.get.size == node.size - 1
      }

  test("deleteAt and modifyAt are consistent"):
    forAll: (p: (Node[Int], List[Int]), f: Int => Int) =>
      val (node, path) = p
      node.deleteAt(path).isDefined == node.modifyAt(path, Tree.lift(f)).isDefined

  test("mapAccuml without using accumulator is the same as map"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node.mapAccuml_(0)((s, a) => (s, f(a))) == node.map(f)

  test("mapAccuml is the same as mapAccumlOption"):
    forAll: (node: Node[Int], init: Int, f: (Int, Int) => (Int, Int)) =>
      node.mapAccuml_(init)(f).some == node.mapAccumlOption_(init)((s, a) => f(s, a).map(Some(_)))

  test("mapAccuml is the different thatn mapAccumulate if variation is exist"):
    forAll: (node: Node[Int], n: Int, f: (Int, Int) => (Int, Int)) =>
      val x = node.mapAccuml(n)(f)
      val y = node.mapAccumulate(n)(f)
      node.variationIsEmpty || x != y

  test("mapAccuml with sum"):
    forAll: (node: Node[Int], n: Int) =>
      val s1 = node.mapAccuml_(n.toLong)((s, a) => (s, s + a.toLong)).sumAll
      val s2 = node.foldMap(_.toLong) + node.size * n.toLong
      s1 == s2

  given Monoid[Long] with
    def empty                     = 0L
    def combine(x: Long, y: Long) = x + y

  given Monoid[Int] with
    def empty                   = 0
    def combine(x: Int, y: Int) = x + y

  extension [A](node: Node[A])
    def variationsCount: Long =
      node.child.foldLeft(node.variations.foldMap(_.size))((acc, v) => acc + v.variationsCount)

    def variationIsEmpty: Boolean =
      node.child.foldLeft(node.variations.isEmpty)((acc, v) => acc || v.variationIsEmpty)
