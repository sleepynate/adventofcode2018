package adventofcode

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day6Suite extends FunSuite{

  import Day6._

  val testStrings = Set("1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9")

  test("can parse points") {
    val coordinates = testStrings.map(Coordinate.fromString)

    coordinates shouldBe Set(Coordinate(1,1), Coordinate(1, 6), Coordinate(8, 3), Coordinate(3, 4), Coordinate(5, 5), Coordinate(8, 9))
  }

  test("can find infinite points") {
    val boundary = findBoundary(testStrings.map(Coordinate.fromString))
    boundary.outside shouldBe Set(Coordinate(1,6), Coordinate(1,1), Coordinate(8,9), Coordinate(8,3))
  }

  test("can find distance between two coordinates") {
    Coordinate(1,1).distance(Coordinate(1,6)) shouldBe 5
    Coordinate(1,1).distance(Coordinate(3,4)) shouldBe 5
    Coordinate(3,4).distance(Coordinate(1,1)) shouldBe 5
  }

  test("Can find closest coordinate") {
    val boundary = findBoundary(testStrings.map(Coordinate.fromString))
    findClosestCoordinate(Coordinate(0,0), boundary) shouldBe Some(Coordinate(1,1))
    findClosestCoordinate(Coordinate(0,4), boundary) shouldBe None
    findClosestCoordinate(Coordinate(5,1), boundary) shouldBe None
    findClosestCoordinate(Coordinate(5,4), boundary) shouldBe Some(Coordinate(5,5))
  }

  test("Can find closest coords for all points in plane") {
    val bigMap = findAllPointsInPlane(testStrings.map(Coordinate.fromString))
    largestLocalArea(bigMap)._1 shouldBe Coordinate(5,5)
    largestLocalArea(bigMap)._2 shouldBe 17
  }
  test("Can solve part 1") {
    // do we need to eliminate outer boundary points? apparently not.
    val bigMap = findAllPointsInPlane(getInput.map(Coordinate.fromString).toSet)
    largestLocalArea(bigMap)._2 shouldBe 4186
  }
}
