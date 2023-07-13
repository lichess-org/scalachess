package chess

import cats.Eq
import cats.derived.*

enum Side derives Eq:
  case KingSide, QueenSide

  inline def fold[A](inline k: A, inline q: A): A = if isKingSide then k else q

  def unary_! = fold(QueenSide, KingSide)

  lazy val castledKingFile: File = fold(File.G, File.C)
  lazy val castledRookFile: File = fold(File.F, File.D)

  private lazy val isKingSide = this == Side.KingSide

object Side:

  val all = List(KingSide, QueenSide)

  def kingRookSide(king: Square, rook: Square): Option[Side] =
    Option.when(king.onSameRank(rook)):
      if king ?> rook then QueenSide else KingSide
