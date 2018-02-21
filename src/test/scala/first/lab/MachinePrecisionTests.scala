package first.lab

import org.scalatest.FlatSpec

class MachinePrecisionTests extends FlatSpec {
  "When machinePrecision is called" should " return 10 ^ (-15) " in {
    assert(MachinePrecision.machinePrecision == Math.pow(10, -15))
  }

  "Machine precision " should " return false on addition's associativity " in {
    assert(!MachinePrecision.isAdditionNotAssociativeWithMachinePrecision)
  }

}
