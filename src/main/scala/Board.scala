package chess

import Pos.posAt
import scalaz.Validation.FlatMap._
import variant.{ Variant, Crazyhouse }

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None) {

  import implicitFailures._

  def apply(at: Pos): Option[Piece] = pieces get at

  def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

  lazy val actors: Map[Pos, Actor] = pieces map {
    case (pos, piece) => (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[List[Actor]] = Color Map { color =>
    actors.values filter (_.color == color) toList
  }

  def rolesOf(c: Color): List[Role] = pieces.values.toList collect {
    case piece if piece.color == c => piece.role
  }

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces collect {
    case (pos, Piece(color, King)) => color -> pos
  } toMap

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.white.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean = kingPosOf(c) exists { kingPos =>
    variant.kingThreatened(this, !c, kingPos, _.role != King)
  }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Valid[Board]*): Valid[Board] =
    actions.foldLeft(success(this): Valid[Board])(_ flatMap _)

  def place(piece: Piece) = new {
    def at(at: Pos): Valid[Board] =
      if (pieces contains at) failure("Cannot place at occupied " + at)
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
    piece ← pieces get orig
    takenPos = taking getOrElse dest
    if (pieces contains takenPos)
  } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  def move(orig: Pos) = new {
    def to(dest: Pos): Valid[Board] = {
      if (pieces contains dest) failure("Cannot move to occupied " + dest)
      else pieces get orig map { piece =>
        copy(pieces = (pieces - orig) + ((dest, piece)))
      } toSuccess ("No piece at " + orig + " to move")
    }
  }

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces collect { case (pos, piece) if piece is color => pos } toSet
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def promote(pos: Pos): Option[Board] = for {
    pawn ← apply(pos)
    if (pawn is Pawn)
    b2 ← take(pos)
    b3 ← b2.place(pawn.color.queen, pos)
  } yield b3

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board = {
    copy(variant = v)
  }

  def withCrazyData(data: Crazyhouse.Data) = copy(crazyData = Some(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board = withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Crazyhouse.Data.init)

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

  import Pos._

  def apply(pieces: Traversable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init
}
