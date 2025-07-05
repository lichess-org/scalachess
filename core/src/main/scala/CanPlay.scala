package chess

import cats.syntax.all.*
import cats.{ Foldable, Traverse }
import chess.CanPlay.makeError
import chess.format.Uci
import chess.format.pgn.{ Parser, SanStr }

import scala.annotation.targetName

/**
 * a typeclass that can apply a Moveable and produce a new state and a MoveOrDrop.
*/
trait CanPlay[A]:
  type Step      = (next: A, move: MoveOrDrop, ply: Ply)
  type Result[B] = (state: A, moves: List[B], error: Option[ErrorStr])
  extension (a: A)
    /**
     * Play a move and return the new state and the move or drop that was played.
     */
    def apply[M <: Moveable](move: M): Either[ErrorStr, (next: A, move: MoveOrDrop)]

    /**
     * Parse a SanStr and play the encoded move and return new state and the move or drop that was played.
     */
    def play(san: SanStr): Either[ErrorStr, (next: A, move: MoveOrDrop)] =
      Parser.san(san).flatMap(a.apply)

    /**
     * Parse a String to UCI and play the encoded move and return new state and the move or drop that was played.
     */
    def playUci(uci: String): Either[ErrorStr, (next: A, move: MoveOrDrop)] =
      Uci(uci)
        .toRight(ErrorStr(s"Invalid UCI: $uci"))
        .flatMap(a.apply)

    @targetName("playFromSans")
    def play[F[_]: Traverse](moves: F[SanStr], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      Parser.moves(moves).flatMap(play(_, initialPly)(transform))

    @targetName("playFromUcis")
    def play[F[_]: Traverse](ucis: F[String], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      ucis
        .traverse(move => Uci.apply(move).toRight(ErrorStr(s"Invalid UCI move: $move")))
        .flatMap(play(_, initialPly)(transform))

    @targetName("playFromUcis")
    def play(moves: String, initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      Uci
        .readList(moves)
        .flatMap(play(_, initialPly)(transform))

    def play[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      val (moves = acc, error = error) = playWhileValidReverse(moves, initialPly)(transform)
      error.fold(acc.reverse.asRight)(_.asLeft)

    def playWhileValid[F[_]: Traverse](
        moves: F[SanStr],
        initialPly: Ply
    ): Either[ErrorStr, Result[MoveOrDrop]] =
      Parser.moves(moves).map(playWhileValid(_, initialPly))

    def playWhileValid[F[_]: Traverse](moves: F[SanStr], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, Result[B]] =
      Parser.moves(moves).map(playWhileValid(_, initialPly)(transform))

    def playWhileValid[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply): Result[MoveOrDrop] =
      playWhileValid(moves, initialPly)(_.move)

    def playWhileValid[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): Result[B] =
      val (state, acc, error) = playWhileValidReverse(moves, initialPly)(transform)
      (state = state, moves = acc.reverse, error = error)

    /**
     * Play a sequence of moves while they are valid, returning the state, the moves played and an error if any.
     * The moves are played in reverse order.
     */
    def playWhileValidReverse[F[_]: Traverse](sans: F[SanStr], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, Result[B]] =
      Parser
        .moves(sans)
        .map(moves => playWhileValidReverse(moves, initialPly)(transform))

    /**
     * Parse, play a sequence of SanStr and fold the result into a value B, starting with an initial value.
     * */
    @targetName("foldLeftFromSans")
    def foldLeft[F[_]: Traverse](
        sans: F[SanStr],
        initialPly: Ply
    )[B](empty: B, combine: (B, Step) => B): (result: B, error: Option[ErrorStr]) =
      Parser
        .moves(sans)
        .fold(
          error => (empty, error.some),
          moves => foldLeft(moves, initialPly)(empty, combine)
        )

    /**
     * Play a sequence of moves and fold the result into a value B.
     * */
    def foldLeft[M <: Moveable, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply
    )[B](empty: B, combine: (B, Step) => B): (result: B, error: Option[ErrorStr]) =
      moves.zipWithIndex
        .foldM((a, empty)) { case ((next, acc), (move, index)) =>
          next(move)
            .leftMap(_ => (acc, makeError(initialPly + index, move).some))
            .map { case (next: A, move: MoveOrDrop) =>
              (state = next, result = combine(acc, (next, move, initialPly + index + 1)))
            }
        }
        .fold(identity, (_, acc) => (result = acc, error = none))

    /**
     * Parse, play a sequence of SanStr and fold the result from right to left into a value B, starting with an initial value.
     * */
    @targetName("foldRightFromSans")
    def foldRight[F[_]: Traverse](
        sans: F[SanStr],
        initialPly: Ply
    )[B](empty: B, combine: (Step, B) => B): (result: B, error: Option[ErrorStr]) =
      Parser
        .moves(sans)
        .fold(
          error => (empty, error.some),
          moves => foldRight(moves, initialPly)(empty, combine)
        )

    /**
    * Play a sequence of moves and fold the result from right to left into a value B, starting with an initial value.
    */
    def foldRight[M <: Moveable, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply
    )[B](empty: B, combine: (Step, B) => B): (result: B, error: Option[ErrorStr]) =
      val (_, acc, error) = playWhileValidReverse(moves, initialPly)(identity)
      acc.foldLeft(empty)((acc, step) => combine(step, acc)) -> error

    /**
     * Parse, play a sequence of SanStr and apply a function to each step.
     * */
    @targetName("foreachFromSans")
    def foreach[F[_]: Traverse](
        sans: F[SanStr],
        initialPly: Ply
    )[U](f: Step => U): Option[ErrorStr] =
      Parser
        .moves(sans)
        .fold(
          error => error.some,
          moves => foreach(moves, initialPly)(f)
        )

    /**
     * Play a sequence of moves and and apply a function to each step.
     * */
    def foreach[M <: Moveable, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply
    )[U](f: Step => U): Option[ErrorStr] =
      moves.zipWithIndex
        .foldM(a) { case (next, (move, index)) =>
          next(move)
            .leftMap(_ => makeError(initialPly + index, move).some)
            .map { case (next: A, move: MoveOrDrop) =>
              f((next, move, initialPly + index + 1))
              next
            }
        }
        .fold(identity, _ => none[ErrorStr])

    /**
     * Parse, play a sequence of SanStr and fold the result into Tree[B]
     * */
    @targetName("buildTreeFromSans")
    def buildTree[F[_]: Traverse](
        sans: F[SanStr],
        initialPly: Ply
    )[B](combine: Step => Node[B]): (result: Option[Node[B]], error: Option[ErrorStr]) =
      foldRight(sans, initialPly)(
        none[Node[B]],
        (step, node) => node.fold(combine(step))(node => combine(step).withChild(node.some)).some
      )

    /**
     * Parse, play a sequence of SanStr and fold the result into Tree[B]
     * */
    def buildTree[M <: Moveable, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply
    )[B](combine: Step => Node[B]): (result: Option[Node[B]], error: Option[ErrorStr]) =
      foldRight(moves, initialPly)(
        none[Node[B]],
        (step, node) => node.fold(combine(step))(node => combine(step).withChild(node.some)).some
      )

    def playWhileValidReverse[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): (state: A, moves: List[B], error: Option[ErrorStr]) =
      moves.zipWithIndex
        .foldM((a, List.empty[B])) { case ((next, acc), (move, index)) =>
          next(move)
            .leftMap(_ => (next, acc, makeError(initialPly + index, move).some))
            .map { case (next: A, move: MoveOrDrop) =>
              (state = next, moves = transform((next, move, initialPly + index + 1)) :: acc)
            }
        }
        .fold(identity, (next, acc) => (state = next, moves = acc, error = none))

    /**
    * Play a sequence of moves and return list of played positions including the initial position.
    */
    def playPositions[M <: Moveable, F[_]: Traverse](moves: F[M])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Position]] =
      val result = playWhileValid(moves, Ply.initial)(_.move.after)
      result.error.fold((a.position :: result.moves).asRight)(_.asLeft)

    /**
    * Parse and play a sequence of moves and return list of played positions including the initial position.
    */
    @targetName("playPositionsFromSans")
    def playPositions[F[_]: Traverse](moves: F[SanStr])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Position]] =
      Parser.moves(moves).flatMap(playPositions)

    def playBoards[M <: Moveable, F[_]: Traverse](moves: F[M])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Board]] =
      val result = playWhileValid(moves, Ply.initial)(_.move.after.board)
      result.error.fold((a.position.board :: result.moves).asRight)(_.asLeft)

    @targetName("playBoardsFromSans")
    def playBoards[F[_]: Traverse](moves: F[SanStr])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Board]] =
      Parser.moves(moves).flatMap(playBoards)

    @targetName("playMovesFromSans")
    def playMoves[F[_]: Traverse](moves: F[SanStr]): Either[ErrorStr, List[MoveOrDrop]] =
      Parser.moves(moves).flatMap(playMoves)

    def playMoves[M <: Moveable, F[_]: Traverse](moves: F[M]): Either[ErrorStr, List[MoveOrDrop]] =
      val result = playWhileValid(moves, Ply.initial)(_.move)
      result.error.fold(result.moves.asRight)(_.asLeft)

    /**
    * Validate a sequence of moves. This will return an error if any of the moves is invalid.
    */
    def validate[M <: Moveable, F[_]: Foldable](moves: F[M]): Either[ErrorStr, Unit] =
      moves.foldM(a)((state, move) => state(move).map(_.next)).void

    /**
     * Akin to [[validate]] but from a sequence of SanStr instead
    */
    def validate[F[_]: Traverse](sans: F[SanStr]): Either[ErrorStr, Unit] =
      Parser.moves(sans).flatMap(validate)

    /*
     * Play a sequence of moves and return the last state.
     */
    def forward[M <: Moveable, F[_]: Foldable](moves: F[M]): Either[ErrorStr, A] =
      moves.foldM(a)((state, move) => state(move).map(_.next))

    /*
     * Akin to forward but from a sequence of SanStr instead
     */
    def forward[F[_]: Traverse](moves: F[SanStr]): Either[ErrorStr, A] =
      Parser.moves(moves).flatMap(forward)

object CanPlay:

  inline def makeError(currentPly: Ply, move: Moveable): ErrorStr =
    val moveAt  = currentPly.fullMoveNumber
    val moveStr = move.rawString.getOrElse(move.toString)
    ErrorStr(s"Cannot play $moveStr at move $moveAt by ${currentPly.turn.name}")
