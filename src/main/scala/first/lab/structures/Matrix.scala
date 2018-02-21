package first.lab.structures


case class Matrix[A](rows: List[List[A]]) {
  require(Matrix.isValidMatrix(this))
}

object Matrix {
  def isValidMatrix[A](matrix: Matrix[A]): Boolean = {
    def allRowsOfSameLength(rows: List[List[A]]) =
      rows.forall(r => r.length == matrix.rows.maxBy(m => m.length).length)

    def isRowLengthIsPowerOfTwo(): Boolean =
      Matrix.powsOfTwo(0).contains(matrix.rows.head.length)

    allRowsOfSameLength(matrix.rows) && isRowLengthIsPowerOfTwo()
  }

  def powsOfTwo(from: Int): Stream[Int] = Stream.cons(from * from, powsOfTwo(from + 1))
}
