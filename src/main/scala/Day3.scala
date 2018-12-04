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

    def getOverlap(rects: Seq[Rectangle]):(Set[Int],Set[SquareInch]) = {
      def helper(rects: Seq[Rectangle],
                 acc: Set[SquareInch],
                 overlapping: Set[Int]):(Set[Int], Set[(Int, Int)]) = {
        if (rects.size == 1) (overlapping, acc)
        else {
          val tailSets = rects.tail.map { r => (r.id, rects.head overlap r) }
          val (overlappingIDs, overlappingInches) =
            tailSets.foldLeft((Set.empty[Int], Set.empty[SquareInch])) {
            (a: (Set[Int], Set[(Int, Int)]), overlap: (Int, Set[(Int, Int)])) => {
              if (overlap._2.isEmpty)
                (a._1, a._2)
              else
                (a._1 + overlap._1 + rects.head.id, a._2 union overlap._2)
            }
          }
          helper(rects.tail, overlappingInches union acc, overlappingIDs union overlapping)
        }
      }
      helper(rects, Set(), Set())
    }
  }

  def getInput: List[String] = {
    Source.fromResource("input3.txt").getLines().toList
  }
}

