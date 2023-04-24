package chess

trait HasId[A, Id]:
  def getId(a: A): Id

object HasId:
  given [A]: HasId[A, A] with
    def getId(a: A): A = a
