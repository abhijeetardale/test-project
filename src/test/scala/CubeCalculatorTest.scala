import org.scalatest.{MustMatchers, WordSpec}

class CubeCalculatorTest extends WordSpec with MustMatchers {

  "Calling kaprekarNumber" must {

    "return arranged smallest number" in {
      CubeCalculator.smallest(num = 3141) mustBe 1134
    }

    "return arranged largest number" in {
      CubeCalculator.largest(num = 3141) mustBe 4311
    }

    "repeate the process and return the 6174" in {
      CubeCalculator.kaprekarNumber(num = 3141) mustBe 6174
    }

  }
}
