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
    def ply: Ply
    def isWhiteTurn: Boolean =
      ply.turn.black
    def turnNumber: FullMoveNumber =
      if ply.turn.black then ply.fullMoveNumber else ply.fullMoveNumber - 1

object PgnNodeEncoder:

  extension [A](tree: Tree[A])(using PgnNodeEncoder[A])

    /**
     * render a tree to a PgnStr
     */
    def render: PgnStr =
      PgnStr:
        val builder = new StringBuilder
        render(builder)
        builder.toString

    /**
     * append the rendred PgnStr to the builder
     */
    def render(builder: StringBuilder): Unit =
      render(builder, !tree.value.isWhiteTurn)

    // We force to render turn number for the next black turn when the current value
    // has comment(s) or variation(s) or the rendered string of this value is not compact
    // so, this returns true if the current value is black
    // or the current value is white and has comment(s) or variation(s)
    private def forceTurnNumber =
      !tree.value.isWhiteTurn || (tree.value.hasComment || tree.variations.nonEmpty)

    @annotation.tailrec
    private def render(builder: StringBuilder, forceTurnNumber: Boolean): Unit =
      if tree.isVariation then tree.value.renderVariationComment(builder)
      tree.addTurnNumberPrefix(forceTurnNumber, builder)
      renderValueAndVariations(builder)
      tree.child.match
        case None => ()
        case Some(x) =>
          builder.addOne(' ')
          x.render(builder, tree.forceTurnNumber)

    // Add turn number prefix to the builder if needed
    // if the current value is white, We ignore forceTurnNumber value as
    // it always renders with a turn number and a dot for example: `1. e4`
    // if the current value is black and forceTurnNumber is true it needs to
    // render with a turn number and 3 dots for example: `1... e5`
    private def addTurnNumberPrefix(forceTurnNumber: Boolean, builder: StringBuilder): Unit =
      if tree.value.isWhiteTurn then builder.append(tree.value.turnNumber).append(". ")
      else if forceTurnNumber then builder.append(tree.value.turnNumber).append("... ")

    private def renderValueAndVariations(builder: StringBuilder) =
      tree.value.render(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('(')
        x.render(builder)
        builder.addOne(')')
