package first.lab.structures


case class StrassenMatrix[A: Numeric](rows: List[List[A]]) extends Matrix[A] {
  require(StrassenMatrix.isValidMatrix(this))

  override def rowLength: Int = this.rows.head.length

  override def +++(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    new StrassenMatrix[A](this.rows
      .zipWithIndex
      .map { p =>
        p._1.zip(other.rows(p._2)) // zipping with other corresponding row from other matrix
          .map(v => n.plus(v._1, v._2)) // mapping them plus
      })(n)
  }
}

object StrassenMatrix {
  def isValidMatrix[A](matrix: StrassenMatrix[A]): Boolean = {
    def allRowsOfSameLength(rows: List[List[A]]) =
      rows.forall(r => r.length == matrix.rows.maxBy(m => m.length).length)

    def isRowLengthPowerOfTwo: Boolean =
      StrassenMatrix.powersOfTwo(0)
        .dropWhile(_ < matrix.rows.head.length)
        .head == matrix.rows.head.length

    allRowsOfSameLength(matrix.rows) && isRowLengthPowerOfTwo
  }

  def powersOfTwo(from: Int): Stream[Double] =
    Stream.cons(Math.pow(2, from), powersOfTwo(from + 1))


  def multiplyMatrices[A: Numeric](firstMatrix: StrassenMatrix[A],
                                   secondMatrix: StrassenMatrix[A])
                                  (implicit m: Numeric[A]): StrassenMatrix[A] = {
    type ValueWithIndex = (A, Int)

    def updateSum(sumSoFar: A, currElement: ValueWithIndex, currIndex: Int): A =
      m.plus(sumSoFar, m.times(currElement._1, secondMatrix.rows(currElement._2)(currIndex)))

    def splitIntoRows(values: List[A], rowLength: Int): List[List[A]] = {
      require(values.length % rowLength == 0)

      if (values.isEmpty)
        List.empty
      else
        values.slice(0, rowLength) :: splitIntoRows(values.drop(rowLength), rowLength)
    }

    def rowProduct(row: List[A], currIndex: Int): A =
      row.zipWithIndex.foldRight(m.zero) {
        (curr, acc) =>
          updateSum(acc, curr, currIndex)
      }

    val productStream: List[A] = for {
      row <- firstMatrix.rows
      currIndex <- row.indices.toList
    } yield rowProduct(row, currIndex)

    new StrassenMatrix[A](splitIntoRows(productStream, firstMatrix.rows.head.length))(m)
  }

  def A11[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    new StrassenMatrix[A](List.empty)(n)
  }

  def A12[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    new StrassenMatrix[A](List.empty)(n)
  }

  def A21[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    new StrassenMatrix[A](List.empty)(n)
  }

  def A22[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    new StrassenMatrix[A](List.empty)(n)
  }

  def B11[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    A11(matrix)(n)
  }

  def B12[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    A12(matrix)(n)
  }

  def B21[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    A21(matrix)(n)
  }

  def B22[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    A22(matrix)(n)
  }
}
