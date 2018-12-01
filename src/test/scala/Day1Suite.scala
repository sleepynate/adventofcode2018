package adventofcode

import org.scalatest.FunSuite

class Day1Suite extends FunSuite{

  test("Calculates Frequency Changes from example string") {
    val testString =
      """
        |+1
        |-2
        |+3
        |+1
      """.stripMargin
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

  test("Can get resulting frequency") {
    assert(Day1.frequencyChange(0, Day1.getInput) === 0, "Should be solution")
  }
}
