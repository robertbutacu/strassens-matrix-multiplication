package first.lab.structures

case class RegularMatrix[A: Numeric](rows: List[List[A]]) extends Matrix[A] {
  require(this.rows.forall(_.length == this.rows.maxBy(_.length).length))

  override def rowLength: Int = this.rows.head.length

  override def +++(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.plus)
  }

  override def ---(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.minus)
  }

  private def applyOperation(other: Matrix[A], f: (A, A) => A): Matrix[A] = {
    new RegularMatrix[A](this.rows.zip(other.rows).map(p => p._1.zip(p._2).map(v => f(v._1, v._2))))

  }
}
