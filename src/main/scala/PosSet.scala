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

  private def slidingAttacks(orig: Pos, occupied: PosSet, dirs: Directions): PosSet = {
    val builder = newBuilder
    dirs foreach { dir =>
      var pos: Option[Pos] = Some(orig)
      while (pos.isDefined) {
        pos flatMap dir match {
          case Some(p) =>
            builder += p
            if (occupied.has(p)) pos = None
            else pos = Some(p)
          case None => pos = None
        }
      }
    }
    builder.result()
  }

  private val kingAttackTable: Array[PosSet] = Pos.all.map { orig =>
    slidingAttacks(orig, full, King.dirs)
  }
  private val knightAttackTable: Array[PosSet] = Pos.all.map { orig =>
    slidingAttacks(orig, full, Knight.dirs)
  }
  private val magicAttackTable: Array[PosSet] = {
    val table: Array[PosSet] = new Array(Magic.tableSize)
    Pos.all.foreach { orig =>
      val rookMagic = Magic.rook(orig.index)
      PosSet(rookMagic.mask).subsets().foreach { subset =>
        val idx    = ((rookMagic.factor * subset.bitboard) >>> (64 - 12)).toInt + rookMagic.offset
        val attack = slidingAttacks(orig, subset, Rook.dirs)
        assert(table(idx) == null || table(idx) == attack)
        table(idx) = attack
      }
      val bishopMagic = Magic.bishop(orig.index)
      PosSet(bishopMagic.mask).subsets().foreach { subset =>
        val idx    = ((bishopMagic.factor * subset.bitboard) >>> (64 - 9)).toInt + bishopMagic.offset
        val attack = slidingAttacks(orig, subset, Bishop.dirs)
        assert(table(idx) == null || table(idx) == attack)
        table(idx) = attack
      }
    }
    table
  }

  def kingAttacks(orig: Pos): PosSet   = kingAttackTable(orig.index)
  def knightAttacks(orig: Pos): PosSet = knightAttackTable(orig.index)
  def rookAttacks(orig: Pos, occupied: PosSet): PosSet = {
    val magic = Magic.rook(orig.index)
    val idx   = ((magic.factor * (occupied.bitboard & magic.mask)) >>> (64 - 12)).toInt + magic.offset
    magicAttackTable(idx)
  }
  def bishopAttacks(orig: Pos, occupied: PosSet): PosSet = {
    val magic = Magic.bishop(orig.index)
    val idx   = ((magic.factor * (occupied.bitboard & magic.mask)) >>> (64 - 9)).toInt + magic.offset
    magicAttackTable(idx)
  }
  def queenAttacks(orig: Pos, occupied: PosSet): PosSet =
    rookAttacks(orig, occupied) ^ bishopAttacks(orig, occupied)
}
