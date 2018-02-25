package first.lab

import first.lab.structures.StrassenMatrix

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
    if( n <= minN)
      StrassenMatrix.multiplyMatrices(firstMatrix, secondMatrix)
    else {
      val P1 = 1
      val P2 = 2
      val P3 = 3
      val P4 = 4
      val P5 = 5
      val P6 = 6
      val P7 = 7

      new StrassenMatrix[A](List.empty)
    }
  }
}
