package first.lab

import first.lab.structures.StrassenMatrix
import first.lab.structures.StrassenMatrix._

object StrassenAlgorithm {
  def compute[A: Numeric](firstMatrix: StrassenMatrix[A],
                          secondMatrix: StrassenMatrix[A],
                          n: Int,
                          minN: Int): StrassenMatrix[A] = {
    /*
    if( n <= minN )
      compute P1, P2, ETC ETC
    else
      composeMatrices(strassenAlgorithm(firstMatrix.A11, secondMatrix.B11, n / 2, minN),
                      strassenAlgorithm(firstMatrix.A12, secondMatrix.B12, n / 2, minN),
                      strassenAlgorithm(firstMatrix.A21, secondMatrix.B21, n / 2, minN),
                      strassenAlgorithm(firstMatrix.A22, secondMatrix.B22, n / 2, minN)
                     )
     */
    //new StrassenMatrix[A](List.empty)
    if (n <= minN)
      StrassenMatrix.multiplyMatrices(firstMatrix, secondMatrix)
    else {
      val P1 = compute(
        A11(firstMatrix) +++ A22(firstMatrix),
        B11(secondMatrix) +++ B22(secondMatrix),
        n - 1,
        minN)

      val P2 = compute(A21(firstMatrix) +++ A22(firstMatrix), B11(secondMatrix), n - 1, minN)

      val P3 = compute(A11(firstMatrix), B12(secondMatrix) --- B22(secondMatrix), n - 1, minN)

      val P4 = compute(A22(firstMatrix), B21(secondMatrix) --- B11(secondMatrix), n - 1, minN)

      val P5 = compute(A11(firstMatrix) +++ A12(firstMatrix), B22(secondMatrix), n - 1, minN)

      val P6 = compute(
        A21(firstMatrix) --- A11(firstMatrix),
        B11(secondMatrix) --- B12(secondMatrix),
        n - 1,
        minN)

      val P7 = compute(
        A12(firstMatrix) --- A22(firstMatrix),
        B21(secondMatrix) +++ B22(secondMatrix),
        n - 1,
        minN)

      val C11 = P1 +++ P4 --- P5 +++ P7
      val C21 = P2 +++ P4
      val C12 = P3 +++ P5
      val C22 = P1 +++ P3 --- P2 +++ P6

      println("C11 " + C11)
      println("C21 " + C21)
      println("C12 " + C12)
      println("C22 " + C22)

      new StrassenMatrix[A](List.empty)
    }
  }
}
