package first.lab

object MachinePrecision {
  private def negativePowsOfTen(start: Int): Stream[Double] = {
    require(start >= 0)
    Stream.cons(Math.pow(10, -start), negativePowsOfTen(start + 1))
  }

  private def nrStream(start: Double): Stream[Double] = {
    Stream.cons(start, nrStream(start + 1.0))
  }


  val machinePrecision: Double =
    negativePowsOfTen(0).view.takeWhile(_ + 1.0 != 1).last

  def isAdditionNotAssociativeWithMachinePrecision: Boolean =
    ((1.0 + machinePrecision) + machinePrecision) != (1.0 + (machinePrecision + machinePrecision))

  def firstMulNotAss: List[Double] =
    nrStream(0.0)
      .dropWhile(x => x * (machinePrecision * machinePrecision) == (x * machinePrecision) * machinePrecision)
      .take(1000)
      .filter(x => x * (machinePrecision * machinePrecision) != (x * machinePrecision) * machinePrecision)
      .toList
}
