package chess

opaque type File = Int
object File:

  extension (a: File)
    inline def value: Int = a

    inline infix def >(inline o: File): Boolean  = a > o
    inline infix def <(inline o: File): Boolean  = a < o
    inline infix def >=(inline o: File): Boolean = a >= o
    inline infix def <=(inline o: File): Boolean = a <= o

    inline def offset(delta: Int): Option[File] =
      if -8 < delta && delta < 8 then File(a + delta)
      else None

    inline def char: Char = (97 + a).toChar

    inline def upperCaseChar: Char       = (65 + a).toChar
    inline def toUpperCaseString: String = upperCaseChar.toString
  end extension

  inline def of(inline square: Square): File = square.value & 0x7

  inline def fromChar(inline ch: Char): Option[File] = File(ch.toInt - 97)

  private[chess] def unsafe(value: Int): File = value
  def apply(value: Int): Option[File]         = Option.when(0 <= value && value < 8)(value)

  val A = File.unsafe(0)
  val B = File.unsafe(1)
  val C = File.unsafe(2)
  val D = File.unsafe(3)
  val E = File.unsafe(4)
  val F = File.unsafe(5)
  val G = File.unsafe(6)
  val H = File.unsafe(7)

  val all = List(A, B, C, D, E, F, G, H)
