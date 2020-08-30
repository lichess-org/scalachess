package chess

import scala.collection.{ AbstractIterator, SpecificIterableFactory, View }
import scala.collection.immutable.{ AbstractSet, Set, SetOps, StrictOptimizedSetOps }
import scala.collection.mutable

case class PosSet private (bitboard: Long)
    extends AbstractSet[Pos]
    with SetOps[Pos, Set, PosSet]
    with StrictOptimizedSetOps[Pos, Set, PosSet] {

  override protected def newSpecificBuilder: mutable.Builder[Pos, PosSet] = PosSet.newBuilder
  override protected def fromSpecific(coll: IterableOnce[Pos]): PosSet    = PosSet.fromSpecific(coll)

  def has(elem: Pos): Boolean      = ((1L << elem.index) & bitboard) != 0L
  override def contains(elem: Pos) = has(elem)

  override def excl(elem: Pos) = PosSet(bitboard & ~(1L << elem.index))
  override def incl(elem: Pos) = PosSet(bitboard | (1L << elem.index))

  def xor(that: PosSet): PosSet = PosSet(bitboard ^ that.bitboard)
  def ^(that: PosSet): PosSet   = xor(that)

  override def iterator: Iterator[Pos] =
    new AbstractIterator[Pos] {
      private var bb: Long = bitboard
      def hasNext: Boolean = bb != 0L
      def next(): Pos = {
        val pos = Pos.lsb(bb).iterator.next()
        bb &= bb - 1L
        pos
      }
    }

  override def empty = PosSet.empty

  override def knownSize: Int = java.lang.Long.bitCount(bitboard)
  override def size           = knownSize

  override def className = "PosSet"

  // The following are specializations for optimization.

  override def subsets(): Iterator[PosSet] =
    new AbstractIterator[PosSet] {
      private var subset: Long   = 0L
      private var first: Boolean = true
      def hasNext: Boolean       = subset != 0L || first
      def next(): PosSet = {
        if (hasNext) {
          val result = PosSet(subset)
          first = false
          subset = (subset - bitboard) & bitboard
          result
        } else Iterator.empty.next()
      }
    }

  override def last: Pos = Pos.msb(bitboard).iterator.next()

  override def concat(that: collection.IterableOnce[Pos]): PosSet =
    that match {
      case other: PosSet => PosSet(bitboard | other.bitboard)
      case _             => super.concat(that)
    }
  override def intersect(that: collection.Set[Pos]): PosSet =
    that match {
      case other: PosSet => PosSet(bitboard & other.bitboard)
      case _             => super.intersect(that)
    }
  override def removedAll(that: collection.IterableOnce[Pos]): PosSet =
    that match {
      case other: PosSet => PosSet(bitboard & ~other.bitboard)
      case _             => super.removedAll(that)
    }
  override def diff(that: collection.Set[Pos]): PosSet = removedAll(that)

  override def partition(pred: Pos => Boolean): (PosSet, PosSet) = {
    val left = filter(pred)
    (left, diff(left))
  }

  def map(f: Pos => Pos): PosSet                   = fromSpecific(new View.Map(toIterable, f))
  def flatMap(f: Pos => IterableOnce[Pos]): PosSet = fromSpecific(new View.FlatMap(toIterable, f))
  def collect(pf: PartialFunction[Pos, Pos])       = fromSpecific(super[SetOps].collect(pf).toIterable)
}

object PosSet extends SpecificIterableFactory[Pos, PosSet] {
  override val empty = PosSet(0L)

  val full          = PosSet(-1L)
  val center        = PosSet(Pos.E4, Pos.D4, Pos.E5, Pos.D5)
  val whiteBackRank = (Pos.A1 <-> Pos.H1).to(PosSet)
  val blackBackRank = (Pos.A8 <-> Pos.H8).to(PosSet)

  override def fromSpecific(that: scala.collection.IterableOnce[Pos]): PosSet =
    that match {
      case other: PosSet => other
      case _             => (newBuilder ++= that).result()
    }

  override def newBuilder: mutable.Builder[Pos, PosSet] =
    new mutable.Builder[Pos, PosSet] {
      var bitboard: Long = 0L
      override def clear() = {
        bitboard = 0L
      }
      override def addOne(elem: Pos) = {
        bitboard = bitboard | (1L << elem.index)
        this
      }
      override def result() = PosSet(bitboard)
    }
}
