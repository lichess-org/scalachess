package chess

trait HasId[A, Id]:
  def getId(a: A): Id
  extension [A](a: A) inline def id[Id](using HasId[A, Id]): Id = summon[HasId[A, Id]].getId(a)

object HasId:
  given [A]: HasId[A, A] with
    def getId(a: A): A = a

