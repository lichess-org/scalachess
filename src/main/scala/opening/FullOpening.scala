package chess

final class FullOpening(
  val id: String,
  val name: String,
  val fen: String,
  val uciString: String) {

  def uciList = uciString.split(' ').toList
}
