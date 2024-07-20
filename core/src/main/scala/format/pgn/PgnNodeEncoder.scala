package chess
package format.pgn

/**
 * PgnNodeEncoder,
 * Provide encoding of a node to a string, which is used to render a PGN string
 * from a chess.Tree[A]
 */
trait PgnNodeEncoder[A]:
  extension (a: A)
    def render(builder: StringBuilder): Unit
    def renderVariationComment(builder: StringBuilder): Unit
    def hasComment: Boolean

object PgnNodeEncoder:

  extension [A](tree: Tree[A])(using PgnNodeEncoder[A])

    /**
     * render a tree to a PgnStr
     */
    def toPgnStr(startPly: Ply): PgnStr =
      PgnStr:
        val builder = new StringBuilder
        appendPgnStr(builder, startPly)
        builder.toString

    /**
     * append the rendred PgnStr to the builder
     */
    def appendPgnStr(builder: StringBuilder, ply: Ply): Unit =
      render(builder, !ply.isWhiteTurn, ply)

    // We force to render turn number for the next black turn when the current value
    // has comment(s) or variation(s) or the rendered string of this value is not compact
    // so, this returns true if the current value is black
    // or the current value is white and has comment(s) or variation(s)
    private def forceTurnNumber(ply: Ply) =
      !ply.isWhiteTurn || (tree.value.hasComment || tree.variations.nonEmpty)

    @annotation.tailrec
    private def render(builder: StringBuilder, forceTurnNumber: Boolean, ply: Ply): Unit =
      if tree.isVariation then tree.value.renderVariationComment(builder)
      tree.addTurnNumberPrefix(forceTurnNumber, builder, ply)
      renderValueAndVariations(builder, ply)
      tree.child.match
        case None => ()
        case Some(x) =>
          builder.addOne(' ')
          x.render(builder, tree.forceTurnNumber(ply), ply.next)

    // Add turn number prefix to the builder if needed
    // if the current value is white, We ignore forceTurnNumber value as
    // it always renders with a turn number and a dot for example: `1. e4`
    // if the current value is black and forceTurnNumber is true it needs to
    // render with a turn number and 3 dots for example: `1... e5`
    private def addTurnNumberPrefix(forceTurnNumber: Boolean, builder: StringBuilder, ply: Ply): Unit =
      if ply.isWhiteTurn then builder.append(ply.turnNumber).append(". ")
      else if forceTurnNumber then builder.append(ply.turnNumber).append("... ")

    private def renderValueAndVariations(builder: StringBuilder, ply: Ply) =
      tree.value.render(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('(')
        x.appendPgnStr(builder, ply)
        builder.addOne(')')

  extension (ply: Ply)
    private def isWhiteTurn: Boolean =
      ply.isOdd
    private def turnNumber: FullMoveNumber =
      ply.fullMoveNumber + ply.value % 2 - 1
