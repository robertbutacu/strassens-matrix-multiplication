package first.lab

import first.lab.structures.StrassenMatrix
import org.scalatest.FlatSpec

class MachinePrecisionTests extends FlatSpec {
  lazy val m1 = StrassenMatrix((1 to 2).toList.map(_ => (1 to 2).toList))

  lazy val m2 = StrassenMatrix(m1.rows.map(_.reverse))

  lazy val strassenMatrixTest = StrassenMatrix((1 to 4).toList.map(_ => (1 to 4).toList))

  lazy val strassenMatrixReversedTest = StrassenMatrix(strassenMatrixTest.rows.map(_.reverse))

  lazy val strassenMatrixExpectedResult = StrassenMatrix(
    List(
      List(40, 30, 20, 10),
      List(40, 30, 20, 10),
      List(40, 30, 20, 10),
      List(40, 30, 20, 10)
    ))


  lazy val m3 = StrassenMatrix(
    List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
      List(13, 14, 15, 16)
    )
  )

  lazy val singleElementMatrix = StrassenMatrix(List(List(1)))

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

  "Adding 2 Strassen Matrices of different lengths" should " fail " in {
    assertThrows[IllegalArgumentException](m1 +++ singleElementMatrix)
  }

  "Adding 2 Strassen Matrices of 1 element " should " be fine " in {
    assert(singleElementMatrix +++ singleElementMatrix === StrassenMatrix(List(List(2))))
  }

  "A11 of a Strassen Matrix " should "return the first quarter" in {
    assert(StrassenMatrix.A11(singleElementMatrix) === singleElementMatrix)
    assert(StrassenMatrix.A11(m3) === StrassenMatrix(List(List(1, 2), List(5, 6))))
  }

  "A12 of a Strassen Matrix " should "return the first quarter" in {
    assert(StrassenMatrix.A12(singleElementMatrix) === singleElementMatrix)
    assert(StrassenMatrix.A12(m3) === StrassenMatrix(List(List(3, 4), List(7, 8))))
  }

  "A21 of a Strassen Matrix " should "return the first quarter" in {
    assert(StrassenMatrix.A21(singleElementMatrix) === singleElementMatrix)
    assert(StrassenMatrix.A21(m3) === StrassenMatrix(List(List(9, 10), List(13, 14))))
  }

  "A22 of a Strassen Matrix " should "return the first quarter" in {
    assert(StrassenMatrix.A22(singleElementMatrix) === singleElementMatrix)
    assert(StrassenMatrix.A22(m3) === StrassenMatrix(List(List(11, 12), List(15, 16))))
  }

  "Strassen Algorithm Multiplication " should "return the correct multiplication" in {
    assert(StrassenAlgorithm.compute(strassenMatrixTest, strassenMatrixReversedTest, 1) === strassenMatrixExpectedResult)
  }

  "Strassen Algorithm multiplication " should " fail if the matrix aren't compatible" in {
    assertThrows[IllegalArgumentException](StrassenAlgorithm.compute(strassenMatrixTest, m1, 1))
  }
}
