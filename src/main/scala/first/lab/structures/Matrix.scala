package first.lab.structures

trait Matrix[A] {
  def rows: List[List[A]]
  def rowLength: Int

  def +++(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A]
  def ---(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A]
}
