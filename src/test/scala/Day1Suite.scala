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
    assert(Day1.frequencyChange(0, testString) === 3)
  }

  test("Calculates additional example strings") {
    val s1 =
      """+1
        |+1
        |+1""".stripMargin
    assert(Day1.frequencyChange(0, s1) === 3)
    val s2 =
      """+1
        |+1
        |-2""".stripMargin
    assert(Day1.frequencyChange(0, s2) === 0)
    val s3 =
      """-1
        |-2
        |-3""".stripMargin
    assert(Day1.frequencyChange(0, s3) === -6)
  }

  test("Still finds correct value for historical frequencies") {
    assert(Day1.frequencyChangeHist(0, testString) == 2)
  }

}
