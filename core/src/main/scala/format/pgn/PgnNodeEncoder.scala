package chess
package format.pgn

import cats.syntax.all.*

import scala.annotation.tailrec

/**
 * PgnNodeEncoder,
 * Provide encoding of a node to a string, which is used to render a PGN string
 * from a chess.Tree[A]
 */
trait PgnNodeEncoder[A]:
  extension (a: A)
    def appendSanStr(builder: StringBuilder): Unit
    def appendVariationComment(builder: StringBuilder): Unit
    def hasComment: Boolean

object PgnNodeEncoder:

  extension [A](node: Node[A])
    def toPgn[B, C](
        context: C,
        f: (C, A) => Option[(C, B)],
        startPly: Ply
    )(using PgnNodeEncoder[B]): Option[PgnStr] =
      node
        .mapAccumlOption_(context): (context, a) =>
          f(context, a).fold(context -> none)(_ -> _.some)
        .map(_.toPgnStr(startPly))

  extension [A](tree: Tree[A])
    /**
     * render a tree to a PgnStr
     */
    def toPgnStr(startPly: Ply)(using PgnNodeEncoder[A]): PgnStr =
      PgnStr:
        val builder = new StringBuilder
        appendPgnStr(builder, startPly)
        builder.toString

    /**
     * append the rendred PgnStr to the builder
     */
    def appendPgnStr(builder: StringBuilder, ply: Ply)(using PgnNodeEncoder[A]): Unit =
      render(builder, !ply.isWhiteTurn, ply)

    // We force to render turn number for the next black turn when the current value
    // has comment(s) or variation(s) or the rendered string of this value is not compact
    // so, this returns true if the current value is black
    // or the current value is white and has comment(s) or variation(s)
    private def forceTurnNumber(ply: Ply)(using PgnNodeEncoder[A]): Boolean =
      !ply.isWhiteTurn || (tree.value.hasComment || tree.variations.nonEmpty)

    @tailrec
    private def render(
        builder: StringBuilder,
        forceTurnNumber: Boolean,
        ply: Ply
    )(using PgnNodeEncoder[A]): Unit =
      if tree.isVariation then tree.value.appendVariationComment(builder)
      addTurnNumberPrefix(forceTurnNumber, builder, ply)
      renderValueAndVariations(builder, ply)
      tree.child.match
        case None => ()
        case Some(x) =>
          builder.addOne(' ')
          x.render(builder, tree.forceTurnNumber(ply), ply.next)

    private def renderValueAndVariations(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      tree.value.appendSanStr(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('('): Unit
        x.appendPgnStr(builder, ply)
        builder.addOne(')')

  // Add turn number prefix to the builder if needed
  // if the current value is white, We ignore forceTurnNumber value as
  // it always renders with a turn number and a dot for example: `1. e4`
  // if the current value is black and forceTurnNumber is true it needs to
  // render with a turn number and 3 dots for example: `1... e5`
  private inline def addTurnNumberPrefix(forceTurnNumber: Boolean, builder: StringBuilder, ply: Ply): Unit =
    if ply.isWhiteTurn then builder.append(ply.turnNumber).append(". "): Unit
    else if forceTurnNumber then builder.append(ply.turnNumber).append("... "): Unit

  extension (ply: Ply)
    private inline def isWhiteTurn: Boolean =
      ply.isOdd
    private inline def turnNumber: FullMoveNumber =
      ply.fullMoveNumber.map(_ + ply.value % 2 - 1)
