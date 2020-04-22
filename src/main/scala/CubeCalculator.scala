object CubeCalculator {

  def kaprekarNumber(num: Int): Int = {

    def calculateConstant(startNumber: Int): Int = {

      if (startNumber == 6174) startNumber
      else calculateConstant(largest(startNumber) - smallest(startNumber))

    }

    calculateConstant(num)

  }

  def smallest(num: Int): Int = {
    num.toString.sorted.toInt
  }

  def largest(num: Int): Int = {
    num.toString.sorted.reverse.toInt
  }
}
