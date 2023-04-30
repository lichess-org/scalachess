package chess
package format
package pgn

import org.scalacheck.{ Arbitrary, Gen }
import cats.kernel.Eq

object NodeGen:

  given [A](using Arbitrary[A]): Arbitrary[Node[A]] = Arbitrary(genNode)

  given EQ[A](using Eq[A]): Eq[Node[A]] = Eq.fromUniversalEquals

  def genNode[A](using Arbitrary[A]): Gen[Node[A]] =
    Gen.sized: size =>
      val sqrt = scala.math.sqrt(size).asInstanceOf[Int]
      for
        a <- Arbitrary.arbitrary[A]
        c <- genChild[A](size)
        v <- genVariation[A](sqrt)
      yield Node(a, c, v)

  def genChild[A](size: Int)(using Arbitrary[A]): Gen[Option[Node[A]]] =
    if size == 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode.map(Some(_)))

  def genVariation[A](size: Int)(using Arbitrary[A]): Gen[Option[Node[A]]] =
    if size == 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode.map(Some(_)))
