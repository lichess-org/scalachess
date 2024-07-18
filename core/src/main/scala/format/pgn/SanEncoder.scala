package chess
package format.pgn

trait SanEncoder[A]:
  extension (a: A)
    def render(builder: StringBuilder): Unit
    def isLong: Boolean
    def isBlack: Boolean
    def renderVariationComment(builder: StringBuilder): Unit
    def turnNumber: FullMoveNumber
    def ply: Ply

object SanEncoder:

  extension [A: SanEncoder](tree: Tree[A])

    def isLong = tree.value.isLong || tree.variations.nonEmpty

    def render: String =
      val builder = new StringBuilder
      render(builder)
      builder.toString

    private[pgn] def render(builder: StringBuilder): Unit =
      render(builder, !tree.value.isBlack)

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
      if tree.value.isBlack then
        builder.append(tree.value.turnNumber).append(". ")
        tree.isLong
      else
        if dot then builder.append(tree.value.turnNumber).append("... ")
        false

    private def renderValueAndVariations(builder: StringBuilder) =
      tree.value.render(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('(')
        x.render(builder)
        builder.addOne(')')
