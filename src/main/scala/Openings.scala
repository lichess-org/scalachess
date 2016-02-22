package chess

// case class Opening(code: String, name: String, moves: String) {

//   def fullName = s"$code $name"

//   lazy val moveList = moves.split(' ').toList

//   def firstMove = moveList.headOption

//   lazy val size = moveList.size

//   def familyName = name.takeWhile(',' !=)

//   override def toString = s"$code $name ($size)"
// }

// object Openings {

//   lazy val codeFamily: Map[String, String] = {
//     OpeningDB.db.foldLeft(Map[String, List[String]]()) {
//       case (acc, Opening(code, name, _)) =>
//         val family = name.takeWhile(',' !=)
//         acc + (code -> (family :: acc.getOrElse(code, Nil)))
//     }.flatMap {
//       case (code, families) =>
//         families
//           .groupBy(identity)
//           .mapValues(_.size)
//           .toList.sortBy(-_._2)
//           .headOption
//           .map(_._1)
//           .map(code -> _)
//     }
//   }

//   lazy val familyFirstMove: Map[String, String] = OpeningDB.db.foldLeft(Map[String, String]()) {
//     case (acc, opening) if (acc contains opening.familyName) => acc
//     case (acc, opening) => opening.firstMove.fold(acc) { firstMove =>
//       acc + (opening.familyName -> firstMove)
//     }
//   }

//   lazy val familyMoveList: Map[String, List[String]] = OpeningDB.db.foldLeft(Map[String, List[String]]()) {
//     case (acc, opening) => acc.get(opening.familyName).filter(_.size < opening.size) match {
//       case None => acc + (opening.familyName -> opening.moveList)
//       case _    => acc
//     }
//   }

//   def generals: List[(String, String)] = {
//     OpeningDB.db.map(_.code).distinct.sorted flatMap { code =>
//       val candidates = OpeningDB.db filter (_.code == code)
//       candidates find (_.name endsWith "General") orElse
//         candidates.sortBy(_.size).headOption map {
//           case Opening(a, b, _) => a -> b
//         }
//     }
//   }
// }
