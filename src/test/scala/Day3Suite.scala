package adventofcode

import org.scalatest.FunSuite

class Day3Suite extends FunSuite {
  import Day3._

  test("Can parse rectangle from String") {
    val r = Rectangle.fromString("#1 @ 1,3: 4x4")
    assert(r.id == 1, "Id should be 1")
    assert(r.x == 1, "x should be 1")
    assert(r.y == 3, "y should be 3")
    assert(r.width == 4, "width should be 4")
    assert(r.height == 4, "height should be 4")
  }

  test("Can get squares covered") {
    val r = Rectangle.fromString("#1 @ 1,3: 4x4")
    val sqs = r.squares

    assert(sqs.contains((1,3)), "Squares should contain (1,3)")
    assert(sqs.contains((2,3)), "Squares should contain (2,3)")
    assert(sqs.contains((3,3)), "Squares should contain (3,3)")
    assert(sqs.contains((4,3)), "Squares should contain (4,3)")

    assert(sqs.contains((1,6)), "Squares should contain (1,6)")
    assert(sqs.contains((2,6)), "Squares should contain (2,6)")
    assert(sqs.contains((3,6)), "Squares should contain (3,6)")
    assert(sqs.contains((4,6)), "Squares should contain (4,6)")
  }

  test("Can get overlapping squares from many rectangles") {
    val r1 = Rectangle.fromString("#1 @ 1,3: 4x4")
    val r2 = Rectangle.fromString("#2 @ 3,1: 4x4")
    val r3 = Rectangle.fromString("#3 @ 5,5: 2x2")

    val rectangles = List(r3, r2, r1)
    val overlap = Rectangle.getOverlap(rectangles)

    assert(!overlap.contains((2,3)))
    assert( overlap.contains((3,3)))
    assert( overlap.contains((4,3)))
    assert(!overlap.contains((5,3)))
    assert(!overlap.contains((2,4)))
    assert( overlap.contains((3,4)))
    assert( overlap.contains((4,4)))
    assert(!overlap.contains((5,4)))
    assert(overlap.size == 4, "overlap size should be 4 sq in")

//    val nonOverlap = rectangles.filterNot(r => overlapIDs.contains(r.id))
//
//    assert(overlapIDs.size == 2, "overlap size should be 2")
//    assert(nonOverlap.head.id == 3, "nonoverlap rectangle ID should be 3")
  }

  test("Can get day 3 answer") {
    val rectangles = Day3.getInput.map(Rectangle.fromString)
    val squareInches = Rectangle.getOverlap(rectangles)
    assert(squareInches.size == 112378, "Day 3 part 1 answer")

//    val nonOverlap = rectangles.filterNot(r => overlapIDs.contains(r.id))
//    assert(nonOverlap.head.id == -1, "nonoverlap rectangle ID should be 3")
  }
}
