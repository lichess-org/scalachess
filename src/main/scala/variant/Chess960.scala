package chess
package variant

import scala.util.Random

case object Chess960 extends Variant(
  id = 2,
  key = "chess960",
  name = "Chess960",
  shortName = "960",
  title = "Starting position of the home rank pieces is randomized",
  standardInitialPosition = false) {

  override def pieces = Variant.symmetricRank {
    val size = 8
    type Rank = IndexedSeq[Option[Role]]
    def ?(max: Int) = Random nextInt max
    def empty(rank: Rank, skip: Int): Option[Int] = {
      1 to size find (x => (rank take x count (_.isEmpty)) == skip + 1)
    } map (_ - 1)
    def update(rank: Rank, role: Role)(x: Int): Rank =
      rank.updated(x, role.some)
    def place(rank: Rank, role: Role, x: Int): Option[Rank] =
      empty(rank, x) map update(rank, role)
    val bishops: Rank =
      IndexedSeq.fill(8)(none[Role])
        .updated(2 * ?(4), Bishop.some) // place first bishop
        .updated(2 * ?(4) + 1, Bishop.some) // place second bishop

    val rank = for {
      a1 ← bishops.some
      a2 ← place(a1, Queen, ?(6))
      a3 ← place(a2, Knight, ?(5))
      a4 ← place(a3, Knight, ?(4))
      a5 ← place(a4, Rook, 0)
      a6 ← place(a5, King, 0)
      a7 ← place(a6, Rook, 0)
    } yield a7

    // shame on me.
    // if someone finds a way to generate a chess960 position
    // safely, let me know.
    rank.err("WTF").flatten
  }
}
