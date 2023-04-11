package chess

opaque type File = Int
object File extends OpaqueInt[File]:
  extension (a: File)

    inline def index: Int = a

    inline def offset(delta: Int): Option[File] =
      if (-8 < delta && delta < 8) atIndex(a + delta)
      else None

    inline def char: Char = (97 + a).toChar

    inline def upperCaseChar: Char       = (65 + a).toChar
    inline def toUpperCaseString: String = upperCaseChar.toString
  end extension

  inline def atIndex(index: Int): Option[File] =
    if (0 <= index && index < 8) Some(index)
    else None

  inline def of(inline square: Square): File = square.value & 0x7

  inline def fromChar(inline ch: Char): Option[File] = atIndex(ch.toInt - 97)

  val A = File(0)
  val B = File(1)
  val C = File(2)
  val D = File(3)
  val E = File(4)
  val F = File(5)
  val G = File(6)
  val H = File(7)

  val all = List(A, B, C, D, E, F, G, H)
