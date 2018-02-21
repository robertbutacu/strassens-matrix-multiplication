package first.lab

object MachinePrecision {
  private def negativePowsOfTen(start: Int): Stream[Double] = {
    require(start >= 0)
    Stream.cons(Math.pow(10, -start), negativePowsOfTen(start + 1))
  }

  val machinePrecision: Double =
    negativePowsOfTen(0).takeWhile(_ + 1.0 != 1).last

  def isAdditionNotAssociativeWithMachinePrecision: Boolean =
    ((1.0 + machinePrecision) + machinePrecision) == (1.0 + (machinePrecision + machinePrecision))
}
