package adventofcode

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day7Suite extends FunSuite {

  import Day7._

  val testInput = List("Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  test("can parse instructions") {
    val reqs = testInput.map(Requirement.fromString)
    reqs.head shouldBe Requirement('C', 'A')
    reqs.tail.head shouldBe Requirement('C', 'F')
  }

  test("can get a map of all requirement edges") {
    val em = EdgeMap(testInput.map(Requirement.fromString).toSet)
    em('C') shouldBe List('A', 'F')
    em('F') shouldBe List('E')
    em('A') shouldBe List('B', 'D')
  }

  test("find starting key") {
    val em = EdgeMap(testInput.map(Requirement.fromString).toSet)
    val sk = em.startingKeys
    sk.head shouldBe 'C'
  }

  test("Can solve part 1 test data") {
    val em = EdgeMap(testInput.map(Requirement.fromString).toSet)
    em.ordering.mkString shouldBe "CABDFE"
  }

  test("Can solve part 1") {
    val em = EdgeMap(getInput.map(Requirement.fromString).toSet)
    em.ordering.mkString.length shouldBe 26
    em.ordering.mkString shouldBe "CGKMUWXFAIHSYDNLJQTREOPZBV"
  }
}
