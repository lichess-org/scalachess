package chess

case class MoveMetrics(
    clientLag: Option[Centis] = None,
    clientMoveTime: Option[Centis] = None,
    frameLag: Option[Centis] = None
):

  // Calculate client reported lag given the server's duration for the move.
  def reportedLag(elapsed: Centis): Option[Centis] =
    clientMoveTime.fold(clientLag)(mt => Option(elapsed - mt))

object MoveMetrics:
  val empty: MoveMetrics = MoveMetrics()
