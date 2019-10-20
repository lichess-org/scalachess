package chess

import Pos.posAt
import scala.collection.breakOut
import scalaz.Validation.FlatMap._
import scalaz.Validation.{ failureNel, success }
import variant.{ Variant, Crazyhouse }

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None
) {

  import implicitFailures._

  def apply(at: Pos): Option[Piece] = pieces get at

  def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

  lazy val actors: Map[Pos, Actor] = pieces map {
    case (pos, piece) => (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)
  }

  def rolesOf(c: Color): List[Role] = pieces.values.collect {
    case piece if piece.color == c => piece.role
  }(breakOut)

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces.collect {
    case (pos, Piece(color, King)) => color -> pos
  }(breakOut)

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.white.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean = kingPosOf(c) exists { kingPos =>
    variant.kingThreatened(this, !c, kingPos)
  }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Valid[Board]*): Valid[Board] =
    actions.foldLeft(success(this): Valid[Board])(_ flatMap _)

  def place(piece: Piece) = new {
    def at(at: Pos): Valid[Board] =
      if (pieces contains at) failureNel("Cannot place at occupied " + at)
      else success(copy(pieces = pieces + ((at, piece))))
  }

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Some(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[Board] = pieces get at map { piece =>
    copy(pieces = pieces - at)
  }

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces contains dest) None
    else pieces get orig map { piece =>
      copy(pieces = pieces - orig + ((dest, piece)))
    }

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] = for {
    piece <- pieces get orig
    takenPos = taking getOrElse dest
    if (pieces contains takenPos)
  } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  def move(orig: Pos) = new {
    def to(dest: Pos): Valid[Board] = {
      if (pieces contains dest) failureNel("Cannot move to occupied " + dest)
      else pieces get orig map { piece =>
        copy(pieces = pieces - orig + (dest -> piece))
      } toSuccess ("No piece at " + orig + " to move")
    }
  }

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces.collect { case (pos, piece) if piece is color => pos }(breakOut)
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def promote(pos: Pos): Option[Board] = for {
    pawn <- apply(pos)
    if (pawn is Pawn)
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

  def withCrazyData(data: Crazyhouse.Data) = copy(crazyData = Some(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board = withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Crazyhouse.Data.init)

  def unmovedRooks = UnmovedRooks {
    history.unmovedRooks.pos.filter(pos =>
      apply(pos).exists(piece => piece.is(Rook) && piece.color.backrankY == pos.y))
  }

  def fixCastles: Board = withCastles {
    if (variant.allowsCastling) {
      val wkPos = kingPosOf(White)
      val bkPos = kingPosOf(Black)
      val wkReady = wkPos.fold(false)(_.y == 1)
      val bkReady = bkPos.fold(false)(_.y == 8)
      def rookReady(color: Color, kPos: Option[Pos], left: Boolean) = kPos.fold(false) { kp =>
        actorsOf(color) exists { a =>
          a.piece.is(Rook) && a.pos.y == kp.y && (left ^ (a.pos.x > kp.x)) && history.unmovedRooks.pos(a.pos)
        }
      }
      Castles(
        whiteKingSide = castles.whiteKingSide && wkReady && rookReady(White, wkPos, false),
        whiteQueenSide = castles.whiteQueenSide && wkReady && rookReady(White, wkPos, true),
        blackKingSide = castles.blackKingSide && bkReady && rookReady(Black, bkPos, false),
        blackQueenSide = castles.blackQueenSide && bkReady && rookReady(Black, bkPos, true)
      )
    } else Castles.none
  }

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = pieces.values count (_ == p)
  def count(c: Color): Int = pieces.values count (_.color == c)

  def autoDraw: Boolean = history.fiftyMoves || variant.insufficientWinningMaterial(this)

  def situationOf(color: Color) = Situation(this, color)

  def visual = format.Visual >> this

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = pieces.values.foldLeft(0) {
    case (acc, Piece(color, role)) => Role.valueOf(role).fold(acc) { value =>
      acc + value * color.fold(1, -1)
    }
  }

  override def toString = List(
    variant + " Position after " + history.lastMove,
    visual
  ) mkString "\n"
}

object Board {

  def apply(pieces: Traversable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Traversable[(Pos, Piece)], castles: Castles, variant: Variant): Board =
    Board(pieces.toMap, History(castles = castles), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init
}
