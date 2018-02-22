package first.lab

import first.lab.structures.Matrix
import org.scalatest.FlatSpec

class MachinePrecisionTests extends FlatSpec {
  "When machinePrecision is called" should " return 10 ^ (-15) " in {
    assert(MachinePrecision.machinePrecision == Math.pow(10, -15))
  }

  "Machine precision " should " return false on addition's associativity " in {
    assert(!MachinePrecision.isAdditionNotAssociativeWithMachinePrecision)
  }

  "Calling multiplyMatrices " should " return the multiplication of the matrices " in {
    val m1 = Matrix((1 to 2).toList.map(_ => (1 to 2).toList))

    val m2 = Matrix(m1.rows.map(_.reverse))

    assert(Matrix.multiplyMatrices(m1, m2) === Matrix(List(List(6, 3), List(6, 3))))
  }

}
