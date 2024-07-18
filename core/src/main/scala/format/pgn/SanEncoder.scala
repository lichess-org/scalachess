package chess
package format.pgn

trait SanEncoder[A]:
  extension (a: A)
    def render(builder: StringBuilder): Unit
    def renderVariationComment(builder: StringBuilder): Unit
    def requiredPrefix: Boolean
    def ply: Ply
    def isWhite: Boolean =
      ply.turn.black
    def turnNumber: FullMoveNumber =
      if ply.turn.black then ply.fullMoveNumber else ply.fullMoveNumber - 1

object SanEncoder:

  extension [A: SanEncoder](tree: Tree[A])

    def render: String =
      val builder = new StringBuilder
      render(builder)
      builder.toString

    def render(builder: StringBuilder): Unit =
      render(builder, !tree.value.isWhite)

    private def requiredPrefix = tree.value.requiredPrefix || tree.variations.nonEmpty

    @annotation.tailrec
    private def render(builder: StringBuilder, dot: Boolean): Unit =
      if tree.isVariation then tree.value.renderVariationComment(builder)
      val d = tree.prefix(dot, builder)
      renderValueAndVariations(builder)
      tree.child match
        case None => ()
        case Some(x) =>
          builder.addOne(' ')
          x.render(builder, d)

    private def prefix(dot: Boolean, builder: StringBuilder): Boolean =
      if tree.value.isWhite then
        builder.append(tree.value.turnNumber).append(". ")
        tree.requiredPrefix
      else
        if dot then builder.append(tree.value.turnNumber).append("... ")
        false

    private def renderValueAndVariations(builder: StringBuilder) =
      tree.value.render(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('(')
        x.render(builder)
        builder.addOne(')')
