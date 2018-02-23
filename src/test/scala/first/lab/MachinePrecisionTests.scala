package first.lab

import first.lab.structures.StrassenMatrix
import org.scalatest.FlatSpec

class MachinePrecisionTests extends FlatSpec {
  lazy val m1 = StrassenMatrix((1 to 2).toList.map(_ => (1 to 2).toList))

  lazy val m2 = StrassenMatrix(m1.rows.map(_.reverse))

  "When machinePrecision is called" should " return 10 ^ (-15) " in {
    assert(MachinePrecision.machinePrecision === Math.pow(10, -15))
  }

  "Machine precision " should " return false on addition's associativity " in {
    assert(!MachinePrecision.isAdditionNotAssociativeWithMachinePrecision)
  }

  "Calling multiplyMatrices " should " return the multiplication of the matrices " in {


    assert(StrassenMatrix.multiplyMatrices(m1, m2) === StrassenMatrix(List(List(6, 3), List(6, 3))))
  }

  "Instantiating a Strassen Matrix whose row length is not multiple of 2" should " break " in {
    assertThrows[IllegalArgumentException](StrassenMatrix((1 to 3).toList.map(_ => (1 to 3).toList)))
  }

  "Adding 2 Strassen Matrices " should "return a Strassen matrix" in {
    assert(m1 +++ m1 === StrassenMatrix((1 to 2).toList.map(e => (1 to 2).toList.map(_ * 2))))
  }
}
