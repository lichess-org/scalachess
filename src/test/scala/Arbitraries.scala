package chess

import org.scalacheck.{ Arbitrary, Gen }
import cats.kernel.Eq

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

  given [A](using Arbitrary[A]): Arbitrary[Tree[A]]      = Arbitrary(genNode)
  given [A](using Arbitrary[A]): Arbitrary[Node[A]]      = Arbitrary(genNode)
  given [A](using Arbitrary[A]): Arbitrary[Variation[A]] = Arbitrary(genVariation)

  given treeEq[A](using Eq[A]): Eq[Tree[A]]           = Eq.fromUniversalEquals
  given nodeEq[A](using Eq[A]): Eq[Node[A]]           = Eq.fromUniversalEquals
  given variationEq[A](using Eq[A]): Eq[Variation[A]] = Eq.fromUniversalEquals

  def genNode[A](using Arbitrary[A]): Gen[Node[A]] =
    Gen.sized: size =>
      val sqrt = Math.sqrt(size.toDouble).toInt
      for
        a <- Arbitrary.arbitrary[A]
        c <- genChild[A](size)
        s <- Gen.choose(0, sqrt)
        v <- Gen.listOfN(s, Gen.resize(sqrt, genVariation[A]))
      yield Node(a, c, v)

  def genChild[A](size: Int)(using Arbitrary[A]): Gen[Option[Node[A]]] =
    if size == 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode.map(Some(_)))

  def genVariation[A](using Arbitrary[A]): Gen[Variation[A]] =
    Gen.sized: size =>
      val sqrt = Math.sqrt(size.toDouble).toInt
      for
        a <- Arbitrary.arbitrary[A]
        c <- genChild[A](sqrt)
      yield Variation(a, c)

  private val genBool = Gen.prob(0.5)
  private val castlesGen =
    for
      wks <- genBool
      wqs <- genBool
      bks <- genBool
      bqs <- genBool
    yield Castles(wks, wqs, bks, bqs)
