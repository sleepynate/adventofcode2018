package adventofcode

import scala.io.Source

object Day7 {

  case class Requirement(mustPrecede: Char, step: Char)

  object Requirement {
    val pattern = raw"Step (.) must be finished before step (.) can begin.".r

    def fromString(s: String) = {
      s match {
        case pattern(m ,s) => Requirement(m.head, s.head)
      }
    }
  }

  case class EdgeMap(m: Map[Char, List[Char]]) {

    def apply(c: Char) = m.apply(c)

    val startingKeys = {
      val keys = m.keySet
      val set = m.values.flatten.toSet
      keys.diff(set)
    }

    def ordering: List[Char] = {
      def helper(map: Map[Char, List[Char]], available: Set[Char], acc: List[Char]): List[Char] = {
        if(map.isEmpty)
          available.toList.sorted.reverse ++ acc
        else {
          val nextStep = available.diff(map.values.flatten.toSet).min
          val nextMap = map - nextStep
          val nextAvailable = (available - nextStep ++ map(nextStep)).diff(acc.toSet)
          val nextAcc = nextStep :: acc
          helper(nextMap, nextAvailable, nextAcc)
        }
      }
      helper(m, startingKeys, List.empty[Char]).reverse
    }

  }

  object EdgeMap {
    def apply(reqs: Set[Requirement]) = {
      new EdgeMap(reqs.groupBy(_.mustPrecede).mapValues(_.map(_.step).toList.sorted))
    }

  }

  def getInput = Source.fromResource("input7.txt").getLines()
}
