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
      tree
        .mapAccumlOption_(context) { (context, a) =>
          f(context, a).fold(context -> none)(_ -> _.some)
        }
        .map(_.toPgnStr(startPly))

  extension [A](tree: Tree[A])

    /**
     * Render a tree to a PgnStr
     */
    def toPgnStr(startPly: Ply): PgnNodeEncoder[A] ?=> PgnStr =
      PgnStr {
        val builder = new StringBuilder
        appendPgnStr(builder, startPly)
        builder.toString
      }

    /**
     * Append the rendered PgnStr to the builder
     */
    def appendPgnStr(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      render(builder, forceTurnNumber(ply), ply)

    /**
     * Determines whether to force rendering the turn number
     */
    private def forceTurnNumber(ply: Ply): PgnNodeEncoder[A] ?=> Boolean =
      !ply.isWhiteTurn || tree.value.hasComment || tree.variations.nonEmpty

    @annotation.tailrec
    private def render(
        builder: StringBuilder,
        forceTurnNumber: Boolean,
        ply: Ply
    ): PgnNodeEncoder[A] ?=> Unit =
      if tree.isVariation then tree.value.appendVariationComment(builder)

      tree.addTurnNumberPrefix(forceTurnNumber, builder, ply)
      renderValueAndVariations(builder, ply)

      tree.child match
        case None    => ()
        case Some(x) =>
          builder.append(' ')
          x.render(builder, x.forceTurnNumber(ply), ply.next)

    /**
     * Adds the turn number prefix to the builder if required
     */
    private def addTurnNumberPrefix(forceTurnNumber: Boolean, builder: StringBuilder, ply: Ply): Unit =
      if ply.isWhiteTurn then builder.append(ply.turnNumber).append(". ")
      else if forceTurnNumber then builder.append(ply.turnNumber).append("... ")

    /**
     * Appends the move and variations to the builder
     */
    private def renderValueAndVariations(builder: StringBuilder, ply: Ply): PgnNodeEncoder[A] ?=> Unit =
      tree.value.appendSanStr(builder)
      tree.variations.foreach { x =>
        builder.append(' ').append('(')
        x.appendPgnStr(builder, ply)
        builder.append(')')
      }

  extension (ply: Ply)
    private def isWhiteTurn: Boolean = ply.isOdd
    private def turnNumber: FullMoveNumber =
      ply.fullMoveNumber.map(_ + (if ply.isOdd then 0 else -1))
