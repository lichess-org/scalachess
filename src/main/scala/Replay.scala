package chess

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits.*

import chess.format.pgn.{ Parser, Reader, San, SanStr, Tag, Tags }
import chess.format.{ Fen, Uci }
import chess.variant.Variant

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game):

  lazy val chronoMoves = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop) =
    copy(
      moves = moveOrDrop.left.map(_.applyVariantEffect) :: moves,
      state = moveOrDrop.fold(state.apply, state.applyDrop)
    )

  def moveAtPly(ply: Ply): Option[MoveOrDrop] =
    chronoMoves.lift(ply.value - 1 - setup.startedAtPly.value)

object Replay:

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, Reader.Result] =
    sans.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        Tags(
          List(
            initialFen map { fen =>
              Tag(_.FEN, fen.value)
            },
            variant.some.filterNot(_.standard) map { v =>
              Tag(_.Variant, v.name)
            }
          ).flatten
        )
      )
    }

  private def computeGames(game: Game, sans: List[San]): Validated[String, List[Game]] =
    sans
      .foldLeft(valid[String, List[Game]](List(game))) { (acc, move) =>
        acc andThen { games =>
          val current = games.head
          move(current.situation) map { moveOrDrop =>
            moveOrDrop.fold(game.apply, game.applyDrop) :: games
          }
        }
      }
      .map(_.reverse)

  def games(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, List[Game]] =
    Parser.moves(sans) andThen { moves =>
      computeGames(makeGame(variant, initialFen), moves.value)
    }

  def gameMoveWhileValid(
      sans: Seq[SanStr],
      initialFen: Fen.Epd,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) =

    // @scala.annotation.tailrec
    def mk(g: Game, moves: List[(San, SanStr)]): (List[(Game, Uci.WithSan)], Option[String]) =
      moves match
        case (san, sanStr) :: rest =>
          san(g.situation).fold(
            err => (Nil, err.some),
            moveOrDrop => {
              val newGame = moveOrDrop.fold(g.apply, g.applyDrop)
              val uci     = moveOrDrop.fold(_.toUci, _.toUci)
              mk(newGame, rest) match {
                case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
              }
            }
          )
        case _ => (Nil, None)
    val init = makeGame(variant, initialFen.some)
    Parser
      .moves(sans)
      .fold(
        err => List.empty[(Game, Uci.WithSan)] -> err.some,
        moves => mk(init, moves.value zip sans)
      ) match
      case (games, err) => (init, games, err)

  private def computeSituations[M](
      sit: Situation,
      moves: List[M],
      play: M => Situation => Validated[String, MoveOrDrop]
  ): Validated[String, List[Situation]] =
    moves
      .foldLeft(valid[String, List[Situation]](List(sit))) { (acc, move) =>
        acc andThen { sits =>
          val current = sits.head
          play(move)(current) map { moveOrDrop =>
            Situation(moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter), !current.color) :: sits
          }
        }
      }
      .map(_.reverse)

  @scala.annotation.tailrec
  private def computeReplay(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match
      case Nil => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) match
          case fail: Validated.Invalid[?] => fail
          case Validated.Valid(moveOrDrop) =>
            computeReplay(replay addMove moveOrDrop, rest)

  private def initialFenToSituation(
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Situation = {
    initialFen.flatMap(Fen.read) | Situation(variant)
  } withVariant variant

  def boards(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, List[Board]] = situations(sans, initialFen, variant) map (_ map (_.board))

  def situations(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, List[Situation]] =
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(sans) andThen { moves =>
      computeSituations[San](sit, moves.value, _.apply)
    }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, List[Situation]] =
    computeSituations[Uci](initialFenToSituation(initialFen, variant), moves, _.apply)

  def apply(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[String, Replay] =
    computeReplay(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant,
      atFen: Fen.Epd
  ): Validated[String, Ply] =
    if (Fen.read(variant, atFen).isEmpty) invalid(s"Invalid Fen $atFen")
    else

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: Fen.Epd) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated            = truncateFen(atFen)
      def compareFen(fen: Fen.Epd)  = truncateFen(fen) == atFenTruncated

      @scala.annotation.tailrec
      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Ply): Validated[String, Ply] =
        sans match
          case Nil => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            san(sit) match
              case fail: Validated.Invalid[?] => fail
              case Validated.Valid(moveOrDrop) =>
                val after = moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter)
                val fen   = Fen write Game(Situation(after, ply.color), ply = ply)
                if (compareFen(fen)) Validated.valid(ply)
                else recursivePlyAtFen(Situation(after, !sit.color), rest, ply + 1)

      val sit = initialFen.flatMap { Fen.read(variant, _) } | Situation(variant)

      Parser.moves(sans) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, Ply(1))
      }

  private def makeGame(variant: Variant, initialFen: Option[Fen.Epd]): Game =
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.ply)
