package chess

import format.Visual
import Pos.posAt

case class Board(
    pieces: AlivePieces,
    history: History,
    variant: Variant) {

  import implicitFailures._

  def apply(at: Pos): Option[Piece] = pieces get at

  def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

  lazy val actors: Map[Pos, Actor] = pieces map {
    case (pos, piece) ⇒ (pos, Actor(piece, pos, this))
  }

  lazy val colorActors: Map[Color, List[Actor]] =
    actors.values groupBy (_.color) mapValues (_.toList)

  def rolesOf(c: Color): List[Role] = pieces.values.toList collect {
    case piece if piece.color == c ⇒ piece.role
  }

  def actorsOf(c: Color): List[Actor] = colorActors get c getOrElse Nil

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces collect {
    case (pos, Piece(color, King)) ⇒ color -> pos
  } toMap

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.white.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean = kingPosOf(c) exists { kingPos ⇒
    Actor.threatens(this, !c, kingPos, _.role != King)
  }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board ⇒ Valid[Board]*): Valid[Board] =
    actions.foldLeft(success(this): Valid[Board])(_ flatMap _)

  def place(piece: Piece) = new {
    def at(at: Pos): Valid[Board] =
      if (pieces contains at) failure("Cannot place at occupied " + at)
      else success(copy(pieces = pieces + ((at, piece))))
  }

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Some(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[Board] = pieces get at map { piece ⇒
    copy(pieces = pieces - at)
  }

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces contains dest) None
    else pieces get orig map { piece ⇒
      copy(pieces = pieces - orig + ((dest, piece)))
    }

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] = for {
    piece ← pieces get orig
    takenPos = taking getOrElse dest
    if (pieces contains takenPos)
  } yield copy(pieces = pieces - takenPos - orig + ((dest, piece)))

  def move(orig: Pos) = new {
    def to(dest: Pos): Valid[Board] = {
      if (pieces contains dest) failure("Cannot move to occupied " + dest)
      else pieces get orig map { piece ⇒
        copy(pieces = (pieces - orig) + ((dest, piece)))
      } toSuccess ("No piece at " + orig + " to move")
    }
  }

  lazy val occupation: Map[Color, Set[Pos]] = Color.all map { color ⇒
    (color, pieces collect { case (pos, piece) if piece is color ⇒ pos } toSet)
  } toMap

  lazy val occupations = pieces.keySet

  def promote(pos: Pos): Option[Board] = for {
    pawn ← apply(pos)
    if (pawn is Pawn)
    b2 ← take(pos)
    b3 ← b2.place(pawn.color.queen, pos)
  } yield b3

  def withHistory(h: History): Board = copy(history = h)

  def withVariant(v: Variant): Board = copy(variant = v)

  def updateHistory(f: History ⇒ History) = copy(history = f(history))

  def count(p: Piece): Int = pieces.values count (_ == p)
  def count(c: Color): Int = pieces.values count (_.color == c)

  def autoDraw: Boolean =
    history.positionHashes.size >= 100 || InsufficientMatingMaterial(this)

  def positionHash: PositionHash = {
    import com.roundeights.hasher.Implicits._
    actors.values.map(_.hash).mkString.md5.bytes take Board.positionHashSize
  }

  def situationOf(color: Color) = Situation(this, color)

  def visual = Visual >> this

  def valid(strict: Boolean) = Color.all map rolesOf forall { roles ⇒
    ((roles count (_ == King)) == 1) :: {
      if (strict) List((roles count (_ == Pawn)) <= 8, roles.size <= 16) else Nil
    } forall identity
  }

  override def toString = visual
}

object Board {

  val positionHashSize = 2

  import Pos._

  def apply(pieces: Traversable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board =
    Board(pieces = variant.pieces, variant = variant)

  def empty(variant: Variant): Board =
    Board(Map.empty, History(), variant)
}
