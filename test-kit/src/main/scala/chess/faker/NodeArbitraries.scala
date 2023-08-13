package chess

import org.scalacheck.{ Arbitrary, Gen }
import cats.kernel.Eq

object NodeArbitraries:

  given [A](using Arbitrary[A]): Arbitrary[Tree[A]]      = Arbitrary(Gen.oneOf(genNode, genVariation))
  given [A](using Arbitrary[A]): Arbitrary[Node[A]]      = Arbitrary(genNode)
  given [A](using Arbitrary[A]): Arbitrary[Variation[A]] = Arbitrary(genVariation)

  type NodeWithPath[A] = (Node[A], List[A])
  given [A](using Arbitrary[A]): Arbitrary[NodeWithPath[A]] = Arbitrary(genNodeWithPath)

  given treeEq[A](using Eq[A]): Eq[Tree[A]]           = Eq.fromUniversalEquals
  given nodeEq[A](using Eq[A]): Eq[Node[A]]           = Eq.fromUniversalEquals
  given variationEq[A](using Eq[A]): Eq[Variation[A]] = Eq.fromUniversalEquals

  def genNodeWithPath[A](using Arbitrary[A]) =
    for
      node <- genNode
      path <- genPath(node)
    yield (node, path)

  def genPath[A](node: Node[A]): Gen[List[A]] =
    val prob = if node.variations.isEmpty then 0.90 else 0.6
    Gen
      .prob(prob)
      .flatMap:
        case true => node.child.fold(Gen.const(Nil))(genPath(_)).map(node.value :: _)
        case false =>
          if node.variations.isEmpty
          then Gen.const(Nil)
          else
            Gen
              .prob(0.95)
              .flatMap:
                case true  => Gen.oneOf(node.variations).flatMap(v => genPath(v.toNode))
                case false => Gen.const(node.value :: Nil)

  def genNode[A](using Arbitrary[A]): Gen[Node[A]] =
    Gen.sized: size =>
      val sqrt = size / 2
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
