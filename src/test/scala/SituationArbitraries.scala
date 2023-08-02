package chess

import cats.syntax.all.*
import org.scalacheck.{ Arbitrary, Gen }
import chess.variant.Variant
import chess.variant.Standard

object SituationArbitraries:

  given Arbitrary[Variant]                       = Arbitrary(Gen.oneOf(Variant.list.all))
  given standardTree: Arbitrary[Node[Situation]] = Arbitrary(genNode(Situation(Standard)))

  def genSituations(seed: Situation): Gen[LazyList[Situation]] =
    if seed.end then Gen.const(LazyList(seed))
    else
      for
        situation  <- Gen.oneOf(seed.legalMoves.map(_.situationAfter))
        situations <- genSituations(situation)
      yield situation #:: situations

  def genNextSituation(seed: Situation): Gen[Option[Situation]] =
    if seed.end then Gen.const(None)
    else Gen.oneOf(seed.legalMoves.map(_.situationAfter)).map(_.some)

  def genMainline(seed: Situation): Gen[Node[Situation]] =
    genSituations(seed).map(Tree.build(_).get)

  def genTree(seed: Situation): Gen[Node[Situation]] =
    genSituations(seed).map(Tree.build(_).get)

  def genNode(seed: Situation, variations: List[Situation] = Nil): Gen[Node[Situation]] =
    val nextSeeds = seed.legalMoves.map(_.situationAfter)
    if nextSeeds.isEmpty then Gen.const(Node(seed, None, Nil))
    else
      Gen.sized: size =>
        for
          a <- Gen.oneOf(nextSeeds)
          childVariations = nextSeeds.filter(_ != a)
          c <- genChild(seed, childVariations, size)
          v <- genVariations(variations)
        yield Node(seed, c, v)

  def sequence[A](xs: List[Gen[A]]): Gen[List[A]] =
    xs.foldRight(Gen.const(Nil: List[A]))((x, acc) => x.flatMap(a => acc.map(a :: _)))

  def pick[A](seeds: List[A]): Gen[List[A]] =
    Gen.choose(0, seeds.size / 8).flatMap(Gen.pick(_, seeds).map(_.toList))

  def genVariations(seeds: List[Situation]): Gen[List[Variation[Situation]]] =
    if seeds.isEmpty then Gen.const(Nil)
    else
      Gen.sized: size =>
        for
          ss <- pick(seeds)
          vg = ss.map(genVariation)
          vs <- sequence(vg)
        yield vs

  def genVariation(value: Situation): Gen[Variation[Situation]] =
    val nextSeeds = value.legalMoves.map(_.situationAfter)
    if nextSeeds.isEmpty then Gen.const(Variation(value, None))
    else
      Gen.sized: size =>
        val sqrt = Math.sqrt(size.toDouble).toInt
        for
          seed <- Gen.oneOf(nextSeeds)
          variations = nextSeeds.filter(_ != seed)
          c <- genChild(seed, variations, sqrt)
        yield Variation(value, c)

  def genChild(value: Situation, variations: List[Situation], size: Int): Gen[Option[Node[Situation]]] =
    if size == 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode(value, variations).map(Some(_)))
