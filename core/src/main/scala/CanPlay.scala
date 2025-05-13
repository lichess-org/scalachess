package chess

import cats.syntax.all.*
import cats.{ Foldable, Traverse }
import chess.CanPlay.makeError
import chess.format.pgn.{ Parser, SanStr }

trait CanPlay[A]:
  type Step      = (next: A, move: MoveOrDrop)
  type Result[B] = (state: A, moves: List[B], error: Option[ErrorStr])
  extension (a: A)
    /**
     * Play a move and return the new state and the move or drop that was played.
     */
    def play(move: Moveable): Either[ErrorStr, Step]

    def playMoves[A <: Moveable, B, F[_]: Traverse](moves: F[A]): Result[MoveOrDrop] =
      playMoves(moves, Ply.initial, _.move)

    def playMovesReverse[A <: Moveable, B, F[_]: Traverse](moves: F[A]): Result[MoveOrDrop] =
      playMovesReverse(moves, Ply.initial, _.move)

    def playMoves[A <: Moveable, B, F[_]: Traverse](moves: F[A], initialPly: Ply, f: Step => B): Result[B] =
      val (state, acc, error) = playMovesReverse(moves, initialPly, f)
      (state = state, moves = acc.reverse, error = error)

    def playSans[F[_]: Traverse](moves: F[SanStr]): Either[ErrorStr, Result[MoveOrDrop]] =
      Parser.moves(moves).map(a.playMoves(_, Ply.initial, _.move))

    def playMovesReverse[M <: Moveable, B, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply,
        f: Step => B
    ): (state: A, moves: List[B], error: Option[ErrorStr]) =
      moves.zipWithIndex
        .foldM((a, List.empty[B])) { case ((next, acc), (move, index)) =>
          next
            .play(move)
            .bimap(
              _ => (next, acc, makeError(initialPly + index, move).some),
              step => (state = step.next, moves = f(step) :: acc)
            )
        }
        .fold(identity, (next, acc) => (state = next, moves = acc, error = none))

    def playPositions[F[_]: Traverse](moves: F[Moveable]): Either[ErrorStr, List[Position]] =
      val (moves = result, error = error) = playMoves(moves, Ply.initial, _.move.after)
      error.fold(result.asRight)(_.asLeft)

    def validate[F[_]: Foldable](moves: F[Moveable]): Either[ErrorStr, Unit] =
      moves.foldM(())((_, move) => a.play(move).void)

    def rewind[F[_]: Foldable](moves: F[Moveable]): Either[ErrorStr, A] =
      moves.foldM(a)((state, move) => state.play(move).map(_.next))

object CanPlay:

  inline def makeError(currentPly: Ply, move: Moveable): ErrorStr =
    val moveAt    = currentPly.fullMoveNumber.value
    val rawString = move.rawString.getOrElse(move.toString())
    ErrorStr(s"Cannot play $rawString at move $moveAt by ${currentPly.turn.name}")

trait Moveable:
  def apply(position: Position): Either[ErrorStr, MoveOrDrop]
  def rawString: Option[String]
