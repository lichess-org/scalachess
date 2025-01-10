        package chess
package format.pgn

import cats.syntax.all.*

/**
 * PgnNodeEncoder,
 * Provides encoding of a node to a string, which is used to render a PGN string
 * from a chess.Tree[A]
 */
trait PgnNodeEncoder[A]:
  extension (a: A)
    def appendSanStr(builder: StringBuilder): Unit
    def appendVariationComment(builder: StringBuilder): Unit
    def hasComment: Boolean

object PgnNodeEncoder:

  extension [A](tree: Node[A])
    def toPgn[B, C](
        context: C,
        f: (C, A) => Option[(C, B)],
        startPly: Ply
    ): PgnNodeEncoder[B] ?=> Option[PgnStr] =
      tree.mapAccumlOption_(context) { (context, a) =>
        f(context, a).fold(context -> none)(_ -> _.some)
      }.map(_.toPgnStr(startPly))

  extension [A](tree: Tree[A])
    
    /**
     * Render a tree to a PgnStr
     */
    def toPgnStr(startPly: Ply): PgnNodeEncoder[A] ?=> PgnStr =
      val builder = new StringBuilder
      appendPgnStr(builder, startPly)
      PgnStr(builder.toString)

    /**
     * Append the rendered PgnStr to the builder
     */
    def appendPgnStr(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      render(builder, ply)

    private def render(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      if tree.isVariation then tree.value.appendVariationComment(builder)
      val forceTurnNumber = shouldForceTurnNumber(ply)
      addTurnNumberPrefix(builder, ply, forceTurnNumber)
      renderValueAndVariations(builder, ply)
      tree.child.foreach { child =>
        builder.addOne(' ')
        child.render(builder, shouldForceTurnNumber(ply), ply.next)
      }

    private def shouldForceTurnNumber(ply: Ply): Boolean =
      !ply.isWhiteTurn || (tree.value.hasComment || tree.variations.nonEmpty)

    private def addTurnNumberPrefix(builder: StringBuilder, ply: Ply, forceTurnNumber: Boolean): Unit =
      if ply.isWhiteTurn then
        builder.append(ply.turnNumber).append(". ")
      else if forceTurnNumber then
        builder.append(ply.turnNumber).append("... ")

    private def renderValueAndVariations(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      tree.value.appendSanStr(builder)
      tree.variations.foreach { variation =>
        builder.addOne(' ').addOne('(')
        variation.appendPgnStr(builder, ply)
        builder.addOne(')')
      }

  extension (ply: Ply)
    private def isWhiteTurn: Boolean = ply.isOdd
    private def turnNumber: FullMoveNumber = ply.fullMoveNumber.map(_ + ply.value % 2 - 1)
