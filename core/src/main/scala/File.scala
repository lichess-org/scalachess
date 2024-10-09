package chess

import chess.bitboard.Bitboard

opaque type File = Int
object File:

  extension (a: File)
    inline def value: Int = a

    inline infix def >(inline o: File): Boolean  = a > o
    inline infix def <(inline o: File): Boolean  = a < o
    inline infix def >=(inline o: File): Boolean = a >= o
    inline infix def <=(inline o: File): Boolean = a <= o

    inline def char: Char = (97 + a).toChar

    inline def upperCaseChar: Char       = (65 + a).toChar
    inline def toUpperCaseString: String = upperCaseChar.toString

    // the bitboard of the file
    def bb: Bitboard = Bitboard.file(value)
  end extension

  inline def of(inline square: Square): File = square.value & 0x7

  inline def fromChar(inline ch: Char): Option[File] = File(ch.toInt - 97)

  def apply(value: Int): Option[File] = Option.when(0 <= value && value < 8)(value)

  val A: File = 0
  val B: File = 1
  val C: File = 2
  val D: File = 3
  val E: File = 4
  val F: File = 5
  val G: File = 6
  val H: File = 7

  val all = List(A, B, C, D, E, F, G, H)
