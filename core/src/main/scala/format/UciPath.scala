package chess
package format

/* Compact representation of a path to a game node,
 * Made from concatenated UciCharPair strings */
opaque type UciPath = String
object UciPath extends OpaqueString[UciPath]:
  def fromId(id: UciCharPair): UciPath = id.toString
  def fromIds(ids: Iterable[UciCharPair]): UciPath = ids.mkString

  extension (e: UciPath)

    def computeIds: Iterator[UciCharPair] = e.grouped(2).flatMap(strToId)
    def ids: List[UciCharPair] = computeIds.toList

    def head: Option[UciCharPair] = strToId(e)

    def parent: UciPath = e.dropRight(2)

    def split: Option[(UciCharPair, UciPath)] = head.map(_ -> e.drop(2))

    inline def isEmpty: Boolean = e.isEmpty
    inline def nonEmpty: Boolean = !isEmpty

    def lastId: Option[UciCharPair] = strToId(e.takeRight(2))

    def +(id: UciCharPair): UciPath = e + id.toString
    def +(more: UciPath): UciPath = e + more

    def prepend(id: UciCharPair): UciPath = id.toString + e

    def intersect(other: UciPath): UciPath =
      val p = e.zip(other).takeWhile(_ == _).map(_._1)
      // `/ 2 * 2` makes sure the size is even. It's necessary!
      p.take(p.size / 2 * 2).mkString

    def depth = e.size / 2

    def debug: String = e.computeIds.map(_.toUci.uci).mkString(" ").trim

  private inline def strToId(inline str: String): Option[UciCharPair] =
    for
      a <- str.headOption
      b <- str.lift(1)
    yield UciCharPair(a, b)

  val root: UciPath = ""
