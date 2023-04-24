package chess

import org.scalacheck.{ Arbitrary, Gen }
import cats.kernel.Eq

import chess.format.pgn.Node

object Arbitraries:

  given Arbitrary[Castles] = Arbitrary(castlesGen)
  given Arbitrary[Color]   = Arbitrary(Gen.oneOf(Color.all))
  given Arbitrary[Side]    = Arbitrary(Gen.oneOf(Side.all))
  given Arbitrary[Role]    = Arbitrary(Gen.oneOf(Role.all))

  given Arbitrary[Piece] = Arbitrary(
    for
      color <- Arbitrary.arbitrary[Color]
      role  <- Arbitrary.arbitrary[Role]
    yield Piece(color, role)
  )

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

  private val genBool = Gen.prob(0.5)
  private val castlesGen =
    for
      wks <- genBool
      wqs <- genBool
      bks <- genBool
      bqs <- genBool
    yield Castles(wks, wqs, bks, bqs)
