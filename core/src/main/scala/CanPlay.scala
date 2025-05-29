package chess

import cats.syntax.all.*
import cats.{ Foldable, Traverse }
import chess.CanPlay.makeError
import chess.format.pgn.{ Parser, SanStr }

import scala.annotation.targetName

/**
 * a typeclass that can apply a Moveable and produce a new state and a MoveOrDrop.
*/
trait CanPlay[A]:
  type Step      = (next: A, move: MoveOrDrop)
  type Result[B] = (state: A, moves: List[B], error: Option[ErrorStr])
  extension (a: A)
    /**
     * Play a move and return the new state and the move or drop that was played.
     */
    def apply[M <: Moveable](move: M): Either[ErrorStr, Step]

    /**
     * Play a sequence of moves and return the new state and a list of MoveOrDrop that were played.
     */
    def play[M <: Moveable, F[_]: Traverse](moves: F[M]): Either[ErrorStr, List[MoveOrDrop]] =
      play(moves, Ply.initial)(_.move)

    /**
     * Akin to play but from a sequence of SanStr instead
     *
     * This will return left if parsing the moves fails
     */
    @targetName("playFromSans")
    def play[F[_]: Traverse](moves: F[SanStr]): Either[ErrorStr, List[MoveOrDrop]] =
      Parser.moves(moves).flatMap(play)

    def play[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      val (moves = acc, error = error) = playWhileValidReverse(moves, initialPly)(transform)
      error.fold(acc.reverse.asRight)(_.asLeft)

    def playWhileValid[M <: Moveable, F[_]: Traverse](moves: F[M]): Result[MoveOrDrop] =
      playWhileValid(moves, Ply.initial)(_.move)

    def playWhileValid[F[_]: Traverse](moves: F[SanStr], initialPly: Ply)[B](
        transform: Step => B
    ): Either[ErrorStr, Result[B]] =
      Parser.moves(moves).map(playWhileValid(_, initialPly)(transform))

    def playWhileValid[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): Result[B] =
      val (state, acc, error) = playWhileValidReverse(moves, initialPly)(transform)
      (state = state, moves = acc.reverse, error = error)

    def playWhileValidReverse[M <: Moveable, F[_]: Traverse](moves: F[M]): Result[MoveOrDrop] =
      playWhileValidReverse(moves, Ply.initial)(_.move)

    def playWhileValidReverse[M <: Moveable, F[_]: Traverse](moves: F[M], initialPly: Ply)[B](
        transform: Step => B
    ): (state: A, moves: List[B], error: Option[ErrorStr]) =
      moves.zipWithIndex
        .foldM((a, List.empty[B])) { case ((next, acc), (move, index)) =>
          next(move)
            .bimap(
              _ => (next, acc, makeError(initialPly + index, move).some),
              step => (state = step.next, moves = transform(step) :: acc)
            )
        }
        .fold(identity, (next, acc) => (state = next, moves = acc, error = none))

    def playPositions[M <: Moveable, F[_]: Traverse](moves: F[M])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Position]] =
      val result = playWhileValid(moves, Ply.initial)(_.move.after)
      result.error.fold((a.position :: result.moves).asRight)(_.asLeft)

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

    /**
    * Validate a sequence of moves. This will return an error if any of the moves are invalid.
    */
    def validate[M <: Moveable, F[_]: Foldable](moves: F[M]): Either[ErrorStr, Unit] =
      moves.foldM(a)((state, move) => state(move).map(_.next)).void

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

    @targetName("playWhileValidReverseFromSans")
    def playWhileValidReverse[F[_]: Traverse](sans: F[SanStr], initialPly: Ply)[B](
        transform: Step => B
    ): (A, List[B], Option[ErrorStr]) =
      Parser
        .moves(sans)
        .fold(
          err => (a, List.empty[B], err.some),
          moves => playWhileValidReverse(moves, initialPly)(transform)
        )

object CanPlay:

  inline def makeError(currentPly: Ply, move: Moveable): ErrorStr =
    val moveAt  = currentPly.fullMoveNumber
    val moveStr = move.rawString.getOrElse(move.toString)
    ErrorStr(s"Cannot play $moveStr at move $moveAt by ${currentPly.turn.name}")
