package chess
package variant

import chess.format.FullFen

case object Chess960
    extends Variant(
      id = Variant.Id(2),
      key = Variant.LilaKey("chess960"),
      uciKey = Variant.UciKey("chess"),
      name = "Chess960",
      shortName = "960",
      title = "Starting position of the home rank pieces is randomized.",
      standardInitialPosition = false
    ):

  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)

  override def valid(situation: Situation, strict: Boolean): Boolean = Standard.valid(situation, strict)

  def pieces = pieces(scala.util.Random.nextInt(960))

  def pieces(position: Int) =
    Variant.symmetricRank:
      positions(position).flatMap(Role.allByForsyth.get)

  def positionNumber(fen: FullFen): Option[Int] =
    fen.value.split(' ') match
      case Array(board, "w", "KQkq", "-", "0", "1") =>
        board.split('/') match
          case Array(rank8, "pppppppp", "8", "8", "8", "8", "PPPPPPPP", rank1) =>
            positionsMap.get(rank8).filter { _ =>
              rank1.zip(rank8).forall { (r1, r8) =>
                r1 != r8 && r1.toLower == r8
              }
            }
          case _ => None
      case _ => None

  def positionToFen(position: Int): Option[FullFen] =
    FullFen.from(
      positions
        .lift(position)
        .map: rank8 =>
          s"$rank8/pppppppp/8/8/8/8/PPPPPPPP/${rank8.toUpperCase} w KQkq - 0 1"
    )

  // https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme#Direct_derivation
  private val positions =
    val row = "12345678"
    val knightPositions =
      for
        i <- 0 to 4
        j <- i + 1 to 4
      yield (i, j)
    def divideWithRemainder(num: Int, by: Int) =
      ((num / by), num % by)
    for
      i <- 0 to 959
      (n2, b1)           = divideWithRemainder(i, 4)
      (n3, b2)           = divideWithRemainder(n2, 4)
      (n4, q)            = divideWithRemainder(n3, 6)
      (knight1, knight2) = knightPositions(n4)
    yield
      val b  = row.updated((2 * b1) + 1, 'b').updated(2 * b2, 'b')
      val bq = b.updated(b.indexWhere(b.filter(_.isDigit)(q) ==), 'q')
      val bqn =
        bq
          .updated(bq.indexWhere(bq.filter(_.isDigit)(knight1) ==), 'n')
          .updated(bq.indexWhere(bq.filter(_.isDigit)(knight2) ==), 'n')
      val bqnr = bqn
        .updated(bqn.indexWhere(_.isDigit), 'r')
        .updated(bqn.lastIndexWhere(_.isDigit), 'r')
      bqnr.updated(bqnr.indexWhere(_.isDigit), 'k')

  private val positionsMap: Map[String, Int] = positions.zipWithIndex.toMap
