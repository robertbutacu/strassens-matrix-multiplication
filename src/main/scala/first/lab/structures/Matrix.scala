package first.lab.structures


case class Matrix[+A: Numeric](rows: List[List[A]]) {
  require(Matrix.isValidMatrix(this))
}

object Matrix {
  def isValidMatrix[A](matrix: Matrix[A]): Boolean = {
    def allRowsOfSameLength(rows: List[List[A]]) =
      rows.forall(r => r.length == matrix.rows.maxBy(m => m.length).length)

    def isRowLengthPowerOfTwo: Boolean =
      Matrix.powersOfTwo(0).dropWhile(_ < matrix.rows.head.length).head == matrix.rows.head.length

    allRowsOfSameLength(matrix.rows) && isRowLengthPowerOfTwo
  }

  def powersOfTwo(from: Int): Stream[Double] = Stream.cons(Math.pow(2, from), powersOfTwo(from + 1))


  def multiplyMatrices[A: Numeric](firstMatrix: Matrix[A], secondMatrix: Matrix[A])(implicit m: Numeric[A]): Matrix[A] = {
    type ValueWithIndex = (A, Int)

    def updateSum(sumSoFar: A, currElement: ValueWithIndex, currIndex: Int): A ={
      println(sumSoFar)
      m.plus(sumSoFar, m.times(currElement._1, secondMatrix.rows(currElement._2)(currIndex)))
    }

    def splitIntoRows(values: List[A], rowLength: Int): List[List[A]] ={
      require(values.length % rowLength == 0)

      if(values.isEmpty)
        List.empty
      else
        values.slice(0, rowLength) :: splitIntoRows(values.drop(rowLength), rowLength)
    }

    val productStream: List[A] = for {
      row <- firstMatrix.rows
      currIndex <- row.indices.toList
    } yield row.zipWithIndex.foldRight(m.zero)((curr, acc) => updateSum(acc, curr, currIndex))

    val resultMatrix = splitIntoRows(productStream, firstMatrix.rows.head.length)

    new Matrix[A](resultMatrix)(m)
  }
}
