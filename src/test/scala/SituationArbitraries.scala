package chess

import cats.syntax.all.*
import org.scalacheck.{ Arbitrary, Gen }
import chess.variant.Variant
import chess.variant.Standard

case class GameTree(init: Situation, startWith: Ply, tree: Option[Node[Move]])

object SituationArbitraries:

  given Arbitrary[Variant]                                 = Arbitrary(Gen.oneOf(Variant.list.all))
  given standardSitutationTree: Arbitrary[Node[Situation]] = Arbitrary(genNode(Situation(Standard)))
  given standardTree: Arbitrary[GameTree]                  = Arbitrary(genTree(Situation(Standard)))

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

  def genTree(seed: Situation): Gen[GameTree] =
    val treeGen =
      if seed.end then Gen.const(None)
      else
        val nextSeeds = seed.legalMoves
        for
          value <- Gen.oneOf(nextSeeds)
          rest = nextSeeds.filter(_ != value)
          variations <- pickSome(rest)
          node       <- genNode(value, variations)
        yield node.some
    treeGen.map(GameTree(seed, Ply.initial, _))

  trait Generator[A]:
    extension (a: A) def next: List[A]

  given Generator[Situation] with
    extension (situation: Situation) def next: List[Situation] = situation.legalMoves.map(_.situationAfter)

  given Generator[Move] with
    extension (move: Move) def next: List[Move] = move.situationAfter.legalMoves

  def genNode[A: Generator](value: A, variations: List[A] = Nil): Gen[Node[A]] =
    val nextSeeds = value.next
    if nextSeeds.isEmpty then Gen.const(Node(value, None, Nil))
    else
      Gen.sized: size =>
        for
          nextChild <- Gen.oneOf(nextSeeds)
          childVariations = nextSeeds.filter(_ != nextChild)
          c <- genChild(nextChild, childVariations, size)
          v <- genVariations(variations)
        yield Node(value, c, v)

  def genVariations[A: Generator](seeds: List[A]): Gen[List[Variation[A]]] =
    if seeds.isEmpty then Gen.const(Nil)
    else
      for
        ss <- pickSome(seeds)
        vg = ss.map(genVariation)
        vs <- sequence(vg)
      yield vs

  def genVariation[A: Generator](value: A): Gen[Variation[A]] =
    val nextSeeds = value.next
    if nextSeeds.isEmpty then Gen.const(Variation(value, None))
    else
      Gen.sized: size =>
        val sqrt = Math.sqrt(size.toDouble).toInt
        for
          seed <- Gen.oneOf(nextSeeds)
          variations = nextSeeds.filter(_ != seed)
          c <- genChild(seed, variations, sqrt)
        yield Variation(value, c)

  def genChild[A: Generator](value: A, variations: List[A], size: Int): Gen[Option[Node[A]]] =
    if size == 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode(value, variations).map(Some(_)))

  def sequence[A](xs: List[Gen[A]]): Gen[List[A]] =
    xs.foldRight(Gen.const(Nil: List[A]))((x, acc) => x.flatMap(a => acc.map(a :: _)))

  def pickSome[A](seeds: List[A]): Gen[List[A]] =
    Gen.choose(0, seeds.size / 10).flatMap(Gen.pick(_, seeds).map(_.toList))
