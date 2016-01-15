package chess
package variant

case object Crazyhouse extends Variant(
  id = 10,
  key = "crazyhouse",
  name = "Crazyhouse",
  shortName = "crazy",
  title = "Every time a piece is captured the capturing player gets a piece of the same type and of their color in their reserve.",
  standardInitialPosition = true) {
}
