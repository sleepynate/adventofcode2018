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

    def getOverlap(rects: Seq[Rectangle]):Map[SquareInch, Int] = {
      val squareInches: Seq[SquareInch] = rects.flatMap(_.squares.toList)
      val squareInchMembership = squareInches.groupBy(identity).mapValues(_.length)
      squareInchMembership.filter(_._2 > 1)
    }
  }

  def getInput: List[String] = {
    Source.fromResource("input3.txt").getLines().toList
  }
}

