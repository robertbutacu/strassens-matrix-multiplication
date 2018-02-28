import first.lab.structures.StrassenMatrix

object Main extends App {
  println(StrassenMatrix((1 to 3).toList.map(_ => (1 to 3).toList)).rows)
}
