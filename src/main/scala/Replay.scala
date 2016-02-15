package chess

import chess.format.Forsyth
import chess.format.pgn.San
import format.pgn.{ Parser, Reader, Tag }
import scalaz.Validation.FlatMap._

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop) = copy(
    moves = moveOrDrop.left.map(_.applyVariantEffect) :: moves,
    state = moveOrDrop.fold(state.apply, state.applyDrop))

  def moveAtPly(ply: Int): Option[MoveOrDrop] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
    moveStrs: List[String],
    initialFen: Option[String],
    variant: chess.variant.Variant): Valid[Replay] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" flatMap { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        List(
          initialFen map { fen => Tag(_.FEN, fen) },
          variant.some.filterNot(_.standard) map { v => Tag(_.Variant, v.name) }
        ).flatten)
    }

  private def recursiveGames(game: Game, sans: List[San]): Valid[List[Game]] =
    sans match {
      case Nil => success(Nil)
      case san :: rest => san(game.situation) flatMap { moveOrDrop =>
        val newGame = moveOrDrop.fold(game.apply, game.applyDrop)
        recursiveGames(newGame, rest) map { newGame :: _ }
      }
    }

  def games(
    moveStrs: List[String],
    initialFen: Option[String],
    variant: chess.variant.Variant): Valid[List[Game]] =
    Parser.moves(moveStrs, variant) flatMap { moves =>
      val game = Game(variant.some, initialFen)
      recursiveGames(game, moves) map { game :: _ }
    }

  type ErrorMessage = String
  def gameWhileValid(
    moveStrs: List[String],
    initialFen: String,
    variant: chess.variant.Variant): (List[Game], Option[ErrorMessage]) = {
    def mk(g: Game, moves: List[San]): (List[Game], Option[ErrorMessage]) = moves match {
      case san :: rest => san(g.situation).fold(
        err => (Nil, err.head.some),
        moveOrDrop => {
          val newGame = moveOrDrop.fold(g.apply, g.applyDrop)
          mk(newGame, rest) match {
            case (next, msg) => (newGame :: next, msg)
          }
        })
      case _ => (Nil, None)
    }
    val init = Game(variant.some, initialFen.some)
    Parser.moves(moveStrs, variant).fold(
      err => Nil -> err.head.some,
      moves => mk(init, moves)
    ) match {
        case (games, err) => (init :: games, err)
      }
  }

  private def recursiveBoards(sit: Situation, sans: List[San]): Valid[List[Board]] =
    sans match {
      case Nil => success(Nil)
      case san :: rest => san(sit) flatMap { moveOrDrop =>
        val after = moveOrDrop.fold(_.afterWithLastMove, _.afterWithLastMove)
        recursiveBoards(Situation(after, !sit.color), rest) map { after :: _ }
      }
    }

  def boards(
    moveStrs: List[String],
    initialFen: Option[String],
    variant: chess.variant.Variant): Valid[List[Board]] = {
    val sit = {
      initialFen.flatMap(Forsyth.<<) | Situation(chess.variant.Standard)
    } withVariant variant
    Parser.moves(moveStrs, sit.board.variant) flatMap { moves =>
      recursiveBoards(sit, moves) map { sit.board :: _ }
    }
  }

  def plyAtFen(
    moveStrs: List[String],
    initialFen: Option[String],
    variant: chess.variant.Variant,
    atFen: String): Valid[Int] =
    if (Forsyth.<<(atFen).isEmpty) s"Invalid FEN $atFen".failureNel
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: String) = fen.split(' ').take(4) mkString " "
      val atFenTruncated = truncateFen(atFen)
      def compareFen(fen: String) = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Valid[Int] =
        sans match {
          case Nil => s"Can't find $atFenTruncated, reached ply $ply".failureNel
          case san :: rest => san(sit) flatMap { moveOrDrop =>
            val after = moveOrDrop.fold(_.afterWithLastMove, _.afterWithLastMove)
            val fen = Forsyth >> Game(after, Color(ply % 2 == 0), turns = ply)
            if (compareFen(fen)) scalaz.Success(ply)
            else recursivePlyAtFen(Situation(after, !sit.color), rest, ply + 1)
          }
        }

      val sit = {
        initialFen.flatMap(Forsyth.<<) | Situation(chess.variant.Standard)
      } withVariant variant

      Parser.moves(moveStrs, sit.board.variant) flatMap { moves =>
        recursivePlyAtFen(sit, moves, 1)
      }
    }
}
