package first.lab

object MachinePrecision {
  private def negativePowsOfTen(start: Int): Stream[Double] = {
    require(start >= 0)
    Stream.cons(Math.pow(10, -start), negativePowsOfTen(start + 1))
  }

  def machinePrecision: Double =
    negativePowsOfTen(0).takeWhile(_ + 1.0 != 1).last

  def isAdditionNotAssociativeWithMachinePrecision: Boolean = {
    val mp = machinePrecision

    ((1.0 + mp) + mp) == (1.0 + (mp + mp))
  }
}
