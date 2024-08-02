package chess

import cats.syntax.all.*
import chess.format.pgn.{ Comment, Glyphs, InitialComments, Move as PgnMove, Pgn, Tags }
import org.scalacheck.Gen

case class GameTree[A](init: Situation, ply: Ply, tree: Option[Node[A]])
case class WithMove[A](move: Move, data: A)

trait Generator[A]:
  extension (a: A) def next: Gen[List[A]]

trait FromMove[A]:
  extension (move: Move) def next(a: Option[A]): Gen[WithMove[A]]

object ChessTreeArbitraries:

  def genSituations(seed: Situation): Gen[LazyList[Situation]] =
    if seed.end then Gen.const(LazyList(seed))
    else
      for
        situation  <- Gen.oneOf(seed.legalMoves.map(_.situationAfter))
        situations <- genSituations(situation)
      yield situation #:: situations

  def genMainline(seed: Situation): Gen[Node[Situation]] =
    genSituations(seed).map(Tree.build(_).get)

  def genPgn(seed: Situation): Gen[Pgn] =
    for
      tree <- genTree(seed)
      pgnTree = tree.tree.map(_.map(_.data))
      comments <- genComments(5)
      pgn = Pgn(Tags.empty, InitialComments(comments), pgnTree, tree.ply.next)
    yield pgn

  def genTree[A](seed: Situation)(using FromMove[A]): Gen[GameTree[WithMove[A]]] =
    genNode(seed).map(GameTree(seed, Ply.initial, _))

  def genNode[A](seed: Situation)(using FromMove[A]): Gen[Option[Node[WithMove[A]]]] =
    if seed.end then Gen.const(None)
    else
      val nextSeeds = seed.legalMoves
      for
        value      <- Gen.oneOf(nextSeeds)
        withMove   <- value.next(none)
        variations <- nextSeeds.filter(_ != value).traverse(_.next(none))
        node       <- genNode(withMove, variations)
      yield node.some

  def genNodeWithPath[A](seed: Situation)(using FromMove[A]): Gen[(Option[Node[WithMove[A]]], List[A])] =
    if seed.end then Gen.const((None, Nil))
    else
      val nextSeeds = seed.legalMoves
      for
        value      <- Gen.oneOf(nextSeeds)
        withMove   <- value.next(none)
        variations <- nextSeeds.filter(_ != value).traverse(_.next(none))
        node       <- genNode(withMove, variations)
        path       <- NodeArbitraries.genPath(node).map(_.map(_.data))
      yield (node.some, path)

  def genComments(size: Int) =
    for
      commentSize <- Gen.choose(0, size)
      xs          <- Gen.listOfN(commentSize, Gen.alphaStr)
      comments = xs.collect { case s if s.nonEmpty => Comment(s) }
    yield comments

  given Generator[Situation] with
    extension (situation: Situation) def next = pickSome(situation.legalMoves.map(_.situationAfter))

  given Generator[Move] with
    extension (move: Move) def next = pickSome(move.situationAfter.legalMoves)

  given [A](using FromMove[A]): Generator[WithMove[A]] with
    extension (move: WithMove[A])
      def next: Gen[List[WithMove[A]]] =
        for
          variations <- pickSome(move.move.situationAfter.legalMoves)
          nextMoves  <- variations.traverse(_.next(move.data.some))
        yield nextMoves

  given FromMove[PgnMove] with
    extension (move: Move)
      def next(m: Option[PgnMove]): Gen[WithMove[PgnMove]] =
        for
          comments <- genComments(5)
          glyphs   <- Gen.someOf(Glyphs.all).map(xs => Glyphs.fromList(xs.toList))
          clock    <- Gen.posNum[Int]
        yield WithMove(move, PgnMove(move.san, comments, glyphs, None, None, clock.some, Nil))

  def genNode[A: Generator](value: A, variations: List[A] = Nil): Gen[Node[A]] =
    value.next.flatMap: nextSeeds =>
      if nextSeeds.isEmpty then Gen.const(Node(value, None, Nil))
      else
        Gen.sized: size =>
          for
            nextChild <- Gen.oneOf(nextSeeds)
            childVariations = nextSeeds.filter(_ != nextChild)
            c <- genChild(nextChild, childVariations, size)
            v <- variations.traverse(genVariation(_))
          yield Node(value, c, v)

  def genVariation[A: Generator](value: A): Gen[Variation[A]] =
    value.next.flatMap: nextSeeds =>
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
    if size <= 0 then Gen.const(None)
    else Gen.resize(size - 1, genNode(value, variations).map(Some(_)))

  def sequence[A](xs: List[Gen[A]]): Gen[List[A]] =
    xs.foldRight(Gen.const(Nil: List[A]))((x, acc) => x.flatMap(a => acc.map(a :: _)))

  def pickSome[A](seeds: List[A], part: Int = 8): Gen[List[A]] =
    if seeds.isEmpty then Gen.const(Nil)
    else
      val max = Math.max(1, seeds.size / part)
      Gen.choose(1, max).flatMap(Gen.pick(_, seeds).map(_.toList))

  extension [A](xs: List[A])
    def traverse[B](f: A => Gen[B]): Gen[List[B]] =
      xs.map(f).foldRight(Gen.const(Nil: List[B]))((x, acc) => x.flatMap(a => acc.map(a :: _)))
