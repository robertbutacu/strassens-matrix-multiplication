package first.lab.structures

case class RegularMatrix[A: Numeric](rows: List[List[A]]) extends Matrix[A] {
  require(this.rows.forall(_.length == this.rows.maxBy(_.length).length))

  override def rowLength: Int = this.rows.head.length

  override def add(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    new RegularMatrix[A](this.rows.zip(other.rows).map(p => p._1.zip(p._2).map(v => n.plus(v._1, v._2))))(n)
  }

}
