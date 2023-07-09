package chess

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import Arbitraries.given

class ByColorTest extends ScalaCheckSuite:

  test("two map should be consistent"):
    forAll: (bc: ByColor[Int], f: Int => String) =>
      bc.map(f) == bc.map(f, f)

  test("mapList == all.map"):
    forAll: (bc: ByColor[Int], f: Int => String) =>
      bc.mapList(f) == bc.all.map(f)

  test("mapReduce == map.reduce"):
    forAll: (bc: ByColor[Int], f: Int => String) =>
      bc.mapReduce(f)(_ + _) == bc.map(f).reduce(_ + _)

  test("mapWithColor == zipColor.map"):
    forAll: (bc: ByColor[Int], f: (Color, Int) => String) =>
      bc.mapWithColor(f) == bc.zipColor.map(f.tupled)

  test("zipColor.map(_._2) == identity"):
    forAll: (bc: ByColor[Int]) =>
      bc.zipColor.map(_._2) == bc

  test("zip = zip(identity)"):
    forAll: (bc: ByColor[Int], other: ByColor[String]) =>
      bc.zip(other) == bc.zip(other, (a, b) => (a, b))

  test("toPair"):
    forAll: (bc: ByColor[Int]) =>
      bc.toPair == (bc(White), bc(Black))

  test("apply"):
    forAll: (bc: ByColor[Int]) =>
      ByColor(bc(White), bc(Black)) == bc

  test("apply and map"):
    forAll: (bc: ByColor[Int], f: Int => String) =>
      val afterMap = bc.map(f)
      bc(White)(f) == afterMap(White) && bc(Black)(f) == afterMap(Black)

  test("update(White).update(Black) == map"):
    forAll: (bc: ByColor[Int], f: Int => Int) =>
      bc.update(White, f).update(Black, f) == bc.map(f)

  test("update(White).update(Black) == update(Black).update(White)"):
    forAll: (bc: ByColor[Int], f: Int => Int) =>
      bc.update(White, f).update(Black, f) == bc.update(Black, f).update(White, f)

  test("faltten == flatMap(identity)"):
    forAll: (bc: ByColor[Option[Int]]) =>
      bc.flatten == bc.flatMap(identity)

  test("fold"):
    forAll: (bc: ByColor[Int], init: String, f: (String, Int) => String) =>
      bc.fold(init)(f) == bc.all.foldLeft(init)(f)

  test("fold == fold ignore color"):
    forAll: (bc: ByColor[Int], init: String, f: (String, Int) => String) =>
      bc.fold(init)(f) == bc.fold(init)((acc, _, i) => f(acc, i))

  test("foreach"):
    forAll: (bc: ByColor[Int], f: Int => String) =>
      var acc = ""
      bc.foreach: i =>
        acc ++= f(i)
      acc == bc.map(f).reduce(_ ++ _)

  test("forall"):
    forAll: (bc: ByColor[Int], f: Int => Boolean) =>
      bc.forall(f) == bc.all.forall(f)

  test("exists"):
    forAll: (bc: ByColor[Int], f: Int => Boolean) =>
      bc.exists(f) == bc.all.exists(f)

  test("findColor && exists"):
    forAll: (bc: ByColor[Int], f: Int => Boolean) =>
      bc.findColor(f).isDefined == bc.all.exists(f)

  test("findColor.apply == find"):
    forAll: (bc: ByColor[Int], f: Int => Boolean) =>
      bc.findColor(f).map(bc(_)) == bc.find(f)

  test("find"):
    forAll: (bc: ByColor[Int], f: Int => Boolean) =>
      bc.find(f) == bc.all.find(f)

  test("find == collect"):
    forAll: (bc: ByColor[Int], f: Int => Option[String]) =>
      val pf: PartialFunction[Int, String] = { case i if f(i).isDefined => f(i).get }
      bc.find(f) == bc.collect(pf)

  test("swap"):
    forAll: (bc: ByColor[Int]) =>
      bc.swap == ByColor(bc(Black), bc(White))

  test("swap.swap == id"):
    forAll: (bc: ByColor[Int]) =>
      bc.swap.swap == bc

  test("traverse"):
    forAll: (bc: ByColor[Int], f: Int => Option[String]) =>
      bc.traverse(f).map(_.all) == bc.all.traverse(f)
