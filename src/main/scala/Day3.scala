package adventofcode

import scala.io.Source

object Day3 {

  type SquareInch = (Int, Int)

  case class Rectangle(id: Int, x: Int, y: Int, width: Int, height: Int) {

    def squares:Set[SquareInch] = {
      val inches = for {
        xs <- x until (x + width)
        ys <- y until (y + height)
      } yield (xs, ys)
      inches.toSet
    }

    def overlap(r: Rectangle): Set[SquareInch] = squares intersect r.squares
    def overlap(r: Set[SquareInch]): Set[SquareInch] = squares intersect r

  }

  object Rectangle {
    def fromString(s1: String): Rectangle = {
      val fmtStr = raw"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r
      s1 match {
        case fmtStr(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }
    }

    def trackerToSquareInches(a: Array[Int], width: Int) = {
      var s = Set.empty[(Int, Int)]
      var x = 0
      var y = 0
      while (width * y < a.length) {
        while (x < width) {
          if (a(y * width + x) == 1) s = s+((x, y))
          x += 1
        }
        x = 0
        y += 1
      }
      s
    }

    def getOverlap(rects: Seq[Rectangle]):(Set[Int],Set[SquareInch]) = {

      val (biggestX, biggestY) = rects.foldLeft((0,0)) {
        (acc, r) =>
          val newX = r.x + r.width
          val newY = r.y + r.height
          val x = if (newX > acc._1) newX else acc._1
          val y = if (newY > acc._2) newY else acc._2
          (x,y)
      }
      val tracker = new Array[Int](biggestX * biggestY)
      def addOverlap(x: Int, y: Int) = tracker(y * biggestX + x) = 1

      def helper(rects: Seq[Rectangle],
                 overlapping: Set[Int]):Set[Int] = {
        if (rects.size == 1) overlapping
        else {
          val tailSets = rects.tail.map { r => (r.id, rects.head overlap r) }
          tailSets.foreach {
            case (_, set) => set.foreach { case (x,y) => addOverlap(x,y) }
          }
          val overlappingIDs = tailSets.foldLeft(Set.empty[Int]) {
            (a: Set[Int], overlap: (Int, Set[(Int, Int)])) => {
              if (overlap._2.isEmpty)
                a
              else
                a + overlap._1 + rects.head.id
            }
          }
          helper(rects.tail, overlappingIDs union overlapping)
        }
      }
      (helper(rects, Set()), trackerToSquareInches(tracker, biggestX))
    }
  }

  def getInput: List[String] = {
    Source.fromResource("input3.txt").getLines().toList
  }
}

