package first.lab.structures


case class StrassenMatrix[A: Numeric](prePaddedRows: List[List[A]]) extends Matrix[A] {
  //require(StrassenMatrix.isValidMatrix(this))

  val rows: List[List[A]] = pad(prePaddedRows)

  def pad(rows: List[List[A]])(implicit n: Numeric[A]): List[List[A]] = {
    val maxLengthRow = rows.maxBy(_.length).length

    def powsOfTwo(from: Int): Stream[Double] = Stream.cons(Math.pow(2, from), powsOfTwo(from + 1))

    val toPadLength = powsOfTwo(0).dropWhile(_ < maxLengthRow.toDouble).head.toInt

    val updatedRows = rows.map(row => row ::: List.fill(toPadLength - row.length)(n.zero))

    val toAddRowsLength = toPadLength - rows.length

    val toAddRows = (1 to toAddRowsLength).toList.map(_ => List.fill(toPadLength)(n.zero))

    updatedRows ::: toAddRows
  }

  override def rowLength: Int = if (this.rows.isEmpty) 0 else this.rows.head.length

  override def +++(other: Matrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.plus)
  }

  override def ---(other: Matrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.minus)
  }

  private def applyOperation(other: Matrix[A], f: (A, A) => A): StrassenMatrix[A] = {
    require(this.rowLength == other.rowLength)

    new StrassenMatrix[A](this.rows
      .zipWithIndex
      .map { p =>
        p._1.zip(other.rows(p._2)) // zipping with other corresponding row from other matrix
          .map(v => f(v._1, v._2)) // mapping them plus
      })
  }

  override def map[B](f: A => B)(implicit n: Numeric[B]): StrassenMatrix[B] = StrassenMatrix(rows.map(_.map(f)))
  override def mapRows[B](f: List[A] => List[B])(implicit n: Numeric[B]): StrassenMatrix[B] = StrassenMatrix(rows.map(f))
}

object StrassenMatrix {
  def isValidMatrix[A](matrix: StrassenMatrix[A]): Boolean = {
    def allRowsOfSameLength(rows: List[List[A]]) =
      rows.forall(currRow => currRow.length == rows.maxBy(r => r.length).length)

    def isRowLengthPowerOfTwo: Boolean =
      if (matrix.rows.isEmpty) true
      else
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

    def computeUpdatedValueForCurrentPosition(l: List[(A, Int)], currIndex: Int) =
      l.foldRight(m.zero) {
        (curr, acc) =>
          updateSum(acc, curr, currIndex)
      }

    val productStream: List[A] = for {
      row <- firstMatrix.rows
      currIndex <- row.indices.toList
      elementsWithIndex = row.zipWithIndex
      valueForCurrentPosition = computeUpdatedValueForCurrentPosition(elementsWithIndex, currIndex)
    } yield valueForCurrentPosition

    new StrassenMatrix[A](splitIntoRows(productStream, firstMatrix.rows.head.length))(m)
  }

  def A11[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    if (matrix.rowLength == 1)
      matrix
    else {
      new StrassenMatrix[A](matrix.rows
        .slice(0, matrix.rowLength / 2)
        .map(_.slice(0, matrix.rowLength / 2)))
    }

  def A12[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    if (matrix.rowLength == 1)
      matrix
    else {
      new StrassenMatrix[A](matrix.rows
        .slice(0, matrix.rowLength / 2)
        .map(_.slice(matrix.rowLength / 2, matrix.rowLength)))
    }

  def A21[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    if (matrix.rowLength == 1)
      matrix
    else {
      new StrassenMatrix[A](matrix.rows
        .slice(matrix.rowLength / 2, matrix.rowLength)
        .map(_.slice(0, matrix.rowLength / 2)))
    }

  def A22[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    if (matrix.rowLength == 1)
      matrix
    else {
      new StrassenMatrix[A](
        matrix.rows
          .slice(matrix.rowLength / 2, matrix.rowLength)
          .map(_.slice(matrix.rowLength / 2, matrix.rowLength))
      )
    }

  def B11[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    A11(matrix)(n)

  def B12[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    A12(matrix)(n)

  def B21[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    A21(matrix)(n)

  def B22[A](matrix: StrassenMatrix[A])(implicit n: Numeric[A]): StrassenMatrix[A] =
    A22(matrix)(n)
}
