import first.lab.StrassenAlgorithm
import first.lab.structures.StrassenMatrix

object Main extends App {
  lazy val m1 = StrassenMatrix((1 to 512).toList.map(_ => (1 to 512).toList))

  lazy val m2 = m1.mapRows(_.reverse)

  StrassenAlgorithm.compute(m1, m2, 32)
}
