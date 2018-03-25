import first.lab.{MachinePrecision, StrassenAlgorithm}
import first.lab.structures.StrassenMatrix

object Main extends App {
  lazy val m1 = StrassenMatrix(
    List(List(-1.5, -1.5, -1.5, -1.5, -1.5),
      List(-1.5, -1.5, -1.5, -1.5, -1.5),
      List(-1.5, -1.5, -1.5, -1.5, -1.5),
      List(-1.5, -1.5, -1.5, -1.5, -1.5),
      List(-1.5, -1.5, -1.5, -1.5, -1.5)
    )
  )

  MachinePrecision.firstMulNotAss.foreach(println)

}
