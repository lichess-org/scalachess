package chess

import variant.{ Crazyhouse, Variant }

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] = {
    val (r, b) = actors.values.toSeq.partition { _.color.red }
    Color.Map(r, b)
  }

  def rolesOf(c: Color): List[Role] =
    pieces.values
      .collect {
        case piece if piece.color == c => piece.role
      }
      .to(List)

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces.collect { case (pos, Piece(color, King)) =>
    color -> pos
  }

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.fold(checkRed, checkBlack)

  lazy val checkRed = checkOf(Red)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(this, !c, kingPos)
    }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Option(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[Board] =
    if (pieces contains at) Option(copy(pieces = pieces - at))
    else None

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces contains dest) None
    else
      pieces get orig map { piece =>
        copy(pieces = pieces - orig + ((dest, piece)))
      }

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for {
      piece <- pieces get orig
      takenPos = taking getOrElse dest
      if pieces contains takenPos
    } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces.collect { case (pos, piece) if piece is color => pos }.to(Set)
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def promote(pos: Pos): Option[Board] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    } yield b3

  def castles: Castles = history.castles

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board = {
    if (v == Crazyhouse)
      copy(variant = v).ensureCrazyData
    else
      copy(variant = v)
  }

  def withCrazyData(data: Crazyhouse.Data)         = copy(crazyData = Option(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board =
    withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Crazyhouse.Data.init)

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.pos.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && piece.color.backRank == pos.rank)
      )
    }

  def fixCastles: Board =
    withCastles {
      if (variant.allowsCastling) {
        val wkPos   = kingPosOf(Red)
        val bkPos   = kingPosOf(Black)
        val wkReady = wkPos.fold(false)(_.rank == Rank.First)
        val bkReady = bkPos.fold(false)(_.rank == Rank.Eighth)
        def rookReady(color: Color, kPos: Option[Pos], left: Boolean) =
          kPos.fold(false) { kp =>
            actorsOf(color) exists { a =>
              a.piece.is(Rook) && a.pos ?- kp && (left ^ (a.pos ?> kp)) && history.unmovedRooks.pos(
                a.pos
              )
            }
          }
        Castles(
          redKingSide = castles.redKingSide && wkReady && rookReady(Red, wkPos, left = false),
          redQueenSide = castles.redQueenSide && wkReady && rookReady(Red, wkPos, left = true),
          blackKingSide = castles.blackKingSide && bkReady && rookReady(Black, bkPos, left = false),
          blackQueenSide = castles.blackQueenSide && bkReady && rookReady(Black, bkPos, left = true)
        )
      } else Castles.none
    }

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = pieces.values count (_ == p)
  def count(c: Color): Int = pieces.values count (_.color == c)

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  def situationOf(color: Color) = Situation(this, color)

  def visual = format.Visual >> this

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.lastMove}\n$visual"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Iterable[(Pos, Piece)], castles: Castles, variant: Variant): Board =
    Board(pieces.toMap, History(castles = castles), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init
}
