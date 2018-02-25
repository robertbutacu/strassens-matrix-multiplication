import first.lab.StrassenAlgorithm
import first.lab.structures.StrassenMatrix

object Main extends App {
  lazy val m1 = StrassenMatrix((1 to 4).toList.map(_ => (1 to 4).toList))

  lazy val m2 = StrassenMatrix(m1.rows.map(_.reverse))

  println(StrassenAlgorithm.compute(
    m1, m2, 1
  ))
}
