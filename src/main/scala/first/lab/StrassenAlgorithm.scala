package first.lab

import first.lab.structures.StrassenMatrix
import first.lab.structures.StrassenMatrix._

object StrassenAlgorithm {
  def compute[A: Numeric](firstMatrix: StrassenMatrix[A],
                          secondMatrix: StrassenMatrix[A],
                          minN: Int): StrassenMatrix[A] = {
    require(firstMatrix.rowLength == secondMatrix.rows.length)

    def multiply(firstMatrix: StrassenMatrix[A],
           secondMatrix: StrassenMatrix[A],
           n: Int,
           minN: Int): StrassenMatrix[A] = {
      require(firstMatrix.rowLength > 0)

      if (n <= minN)
        StrassenMatrix.multiplyMatrices(firstMatrix, secondMatrix)
      else {
        val P1 = multiply(
          A11(firstMatrix) +++ A22(firstMatrix),
          B11(secondMatrix) +++ B22(secondMatrix),
          n / 2,
          minN)

        val P2 = multiply(A21(firstMatrix) +++ A22(firstMatrix), B11(secondMatrix), n / 2, minN)
        val P3 = multiply(A11(firstMatrix), B12(secondMatrix) --- B22(secondMatrix), n / 2, minN)
        val P4 = multiply(A22(firstMatrix), B21(secondMatrix) --- B11(secondMatrix), n / 2, minN)
        val P5 = multiply(A11(firstMatrix) +++ A12(firstMatrix), B22(secondMatrix), n / 2, minN)

        val P6 = multiply(
          A21(firstMatrix) --- A11(firstMatrix),
          B11(secondMatrix) --- B12(secondMatrix),
          n / 2,
          minN)

        val P7 = multiply(
          A12(firstMatrix) --- A22(firstMatrix),
          B21(secondMatrix) +++ B22(secondMatrix),
          n / 2,
          minN)

        val C11 = P1 +++ P4 --- P5 +++ P7
        val C21 = P2 +++ P4
        val C12 = P3 +++ P5
        val C22 = P1 +++ P3 --- P2 +++ P6

        combineMatrices(C11, C12, C21, C22)
      }
    }

    multiply(firstMatrix, secondMatrix, firstMatrix.rowLength, minN)
  }

  private def combineMatrices[A](C11: StrassenMatrix[A],
                                 C12: StrassenMatrix[A],
                                 C21: StrassenMatrix[A],
                                 C22: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    def combineHorizontally(first: StrassenMatrix[A], second: StrassenMatrix[A]): List[List[A]] =
      first.rows
        .zipWithIndex
        .map(r => r._1 ::: second.rows(r._2))

    val upperHalfCombined = combineHorizontally(C11, C12)
    val lowerHalfCombined = combineHorizontally(C21, C22)

    StrassenMatrix(upperHalfCombined ++ lowerHalfCombined)(n)
  }
}
