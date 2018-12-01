package adventofcode

import org.scalatest.FunSuite

class Day1Suite extends FunSuite{

  val testString =
    """
      |+1
      |-2
      |+3
      |+1
    """.stripMargin

  test("Calculates Frequency Changes from example string") {
    assert(Day1.calculateFrequencyChange(testString) === 3)
  }

  test("Calculates additional example strings") {
    val s1 =
      """+1
        |+1
        |+1""".stripMargin
    assert(Day1.calculateFrequencyChange(s1) === 3)
    val s2 =
      """+1
        |+1
        |-2""".stripMargin
    assert(Day1.calculateFrequencyChange(s2) === 0)
    val s3 =
      """-1
        |-2
        |-3""".stripMargin
    assert(Day1.calculateFrequencyChange(s3) === -6)
  }

  test("Still finds correct value for historical frequencies") {
    assert(Day1.findRecurringFrequency(testString) == 2)
    assert(Day1.findRecurringFrequency("+1\n-1") == 0)
    assert(Day1.findRecurringFrequency("+3\n+3\n+4\n-2\n-4") == 10)
    assert(Day1.findRecurringFrequency("-6\n+3\n+8\n+5\n-6") == 5)
    assert(Day1.findRecurringFrequency("+7\n+7\n-2\n-7\n-4") == 14)
  }

}
