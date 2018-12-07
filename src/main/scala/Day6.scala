package adventofcode
import scala.collection.immutable
import scala.io.Source

object Day6 {

  type Maximums = (Int, Int, Int, Int)

  case class Boundary(inside: Set[Coordinate], outside: Set[Coordinate])

  case class Coordinate(x:Int, y:Int) {
    def distance(coordinate: Coordinate): Int = {
      (x - coordinate.x).abs + (y - coordinate.y).abs
    }

  }

  object Coordinate {
    def fromString(s: String): Coordinate = {
      val strings = s.split(", ")
      Coordinate(strings(0).toInt, strings(1).toInt)
    }
  }

  private def findCartesianMinMax(coords: Set[Coordinate]): Maximums = {
    val minx = coords.minBy(_.x).x
    val maxx = coords.maxBy(_.x).x
    val miny = coords.minBy(_.y).y
    val maxy = coords.maxBy(_.y).y
    (minx, maxx, miny, maxy)
  }

  def findBoundary(coords: Set[Coordinate]): Boundary = {
    val mms = findCartesianMinMax(coords)
    val minx = coords.filter(_.x == mms._1)
    val maxx = coords.filter(_.x == mms._2)
    val miny = coords.filter(_.x == mms._3)
    val maxy = coords.filter(_.x == mms._4)
    val outside = Set(minx, maxx, miny, maxy).flatten
    val inside = coords -- outside
    Boundary(inside, outside)
  }

  def findClosestCoordinate(coord: Coordinate, bound: Boundary): Option[Coordinate] = {
    val tuples = (bound.inside union bound.outside).map { c => coord.distance(c) -> c}
    val intToTuples = tuples.groupBy(_._1).mapValues(_.map(_._2))
    val shortestDistance: Set[Coordinate] = intToTuples.minBy(_._1)._2
    if (shortestDistance.size > 1)
      None
    else
      Some(shortestDistance.head)
  }

  def areasAroundCoordinates(coords: Set[Coordinate]) = {
    val (minx, maxx, miny, maxy) = findCartesianMinMax(coords)
    val boundary = findBoundary(coords)
    val allCoords = for {
      x <- minx to maxx
      y <- miny to maxy
    } yield Coordinate(x, y)
    val closestCoords = allCoords.map(c => findClosestCoordinate(c, boundary)).filterNot(_.isEmpty).flatten
    closestCoords.groupBy(identity).mapValues(_.length)
  }

  def findSafeRegion(coords: Set[Coordinate], safeDistance: Int):Set[Coordinate] = {
    val (minx, maxx, miny, maxy) = findCartesianMinMax(coords)
    val boundary = findBoundary(coords)
    val allCoords = for {
      x <- minx to maxx
      y <- miny to maxy
    } yield Coordinate(x, y)
    val tuples: Seq[(Coordinate, Int)] = allCoords.map { c => c -> coords.foldLeft(0) { (acc, coordinate) => acc + c.distance(coordinate) } }
    tuples.filter(_._2 < safeDistance).map(_._1).toSet
  }

  def largestLocalArea(neighbors: Map[Coordinate, Int]) = {
    neighbors.maxBy(_._2)
  }

  def getInput = Source.fromResource("input6.txt").getLines()
}
