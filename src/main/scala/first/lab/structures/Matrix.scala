package first.lab.structures


case class Matrix[A: Numeric](rows: List[List[A]]) {
  require(Matrix.isValidMatrix(this))
}

object Matrix {
  def isValidMatrix[A](matrix: Matrix[A]): Boolean = {
    def allRowsOfSameLength(rows: List[List[A]]) =
      rows.forall(r => r.length == matrix.rows.maxBy(m => m.length).length)

    def isRowLengthPowerOfTwo: Boolean =
      Matrix.powersOfTwo(0).contains(matrix.rows.head.length)

    allRowsOfSameLength(matrix.rows) && isRowLengthPowerOfTwo
  }

  def powersOfTwo(from: Int): Stream[Int] = Stream.cons(from * from, powersOfTwo(from + 1))


  def multiplyMatrices[A: Numeric](firstMatrix: Matrix[A], secondMatrix: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    type ValueWithIndex = (A, Int)

    def updateSum(sumSoFar: A, currElement: ValueWithIndex, currIndex: Int): A =
      n.plus(sumSoFar, n.times(currElement._1, secondMatrix.rows(currElement._2)(currIndex)))

    def splitIntoRows(values: List[A], rowLength: Int): List[List[A]] =
      if(values.isEmpty)
        List.empty
      else
        values.slice(0, rowLength) :: splitIntoRows(values.drop(rowLength), rowLength)

    val productStream: List[A] = for {
      row <- firstMatrix.rows
      currIndex <- row.indices.toList
    } yield row.zipWithIndex.foldRight(n.zero)((curr, acc) => updateSum(acc, curr, currIndex))

    Matrix(splitIntoRows(productStream, firstMatrix.rows.head.length))
  }
}
