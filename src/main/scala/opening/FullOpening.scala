package chess
package opening

final class FullOpening(
    val id: Int,
    val eco: String,
    val name: String,
    val fen: String) {

  override def toString = s"$eco $name"
}
