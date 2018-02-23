package first.lab

import first.lab.structures.StrassenMatrix

object StrassenAlgorithm {
  def compute[A: Numeric](firstMatrix: StrassenMatrix[A],
                          secondMatrix: StrassenMatrix[A],
                          n: Int,
                          minN: Int): StrassenMatrix[A] = {
    new StrassenMatrix[A](List.empty)
  }
}
