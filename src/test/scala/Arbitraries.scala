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
    for
      a <- Arbitrary.arbitrary[A]
      c <- genChild[A]
      v <- genVariation[A]
    yield Node(a, c, v)

  def genChild[A](using Arbitrary[A]): Gen[Option[Node[A]]] =
    Gen.frequency((1, Gen.const(None)), (1, genNode.map(Some(_))))

  def genVariation[A](using Arbitrary[A]): Gen[Option[Node[A]]] =
    Gen.frequency((3, Gen.const(None)), (1, genNode.map(Some(_))))

  private val genBool = Gen.prob(0.5)
  private val castlesGen =
    for
      wks <- genBool
      wqs <- genBool
      bks <- genBool
      bqs <- genBool
    yield Castles(wks, wqs, bks, bqs)
