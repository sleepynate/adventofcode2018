package adventofcode

import scala.io.Source
import scala.language.implicitConversions

object Day2 {
  def word2IndexList(str: String): List[(Char, Int)] = {
    def helper(idx:Int, str: String, lst: List[(Char, Int)]): List[(Char, Int)] = {
      if (str.isEmpty)
        lst
      else
        helper(idx + 1, str.tail, (str.head -> idx) :: lst)
    }
    helper(0, str, Nil)
  }

  def indexSetToString(set: Set[(Char, Int)]) = set.toSeq.sortBy(p => p._2).map(p => p._1).mkString

  def findCommonBoxIDLetters(boxIDs: List[String]): String = {
    val boxSets = boxIDs.toArray.map(bx => word2IndexList(bx).toSet)
    var i = 0
    var j = 1
    var biggestSize = 0
    var biggestReducedString = ""
    while (i < boxSets.length) {
      val left = boxSets(i)
      while (j < boxSets.length) {
        val right = boxSets(j)
        val intersect = left.intersect(right)
        val size = intersect.size
        if (size > biggestSize) {
          biggestSize = size
          biggestReducedString = indexSetToString(intersect)
        }
        j += 1
      }
      i += 1
      j = i + 1
    }
    biggestReducedString
  }


  def findCommonBoxIDLettersFun(boxIDs: List[String]): String = {
    val boxSets = boxIDs.map(bx => word2IndexList(bx).toSet)

    def helper(head: Set[(Char, Int)], tail: List[Set[(Char, Int)]], largest: Set[(Char, Int)]): Set[(Char, Int)] = {
      if (tail.length == 1) {
        val last = head.intersect(tail.head)
        if (last.size > largest.size) last
        else largest
      } else {
        val curr = tail.foldLeft(Set.empty[(Char, Int)]) { (acc, s) =>
          val i = s.intersect(head)
          if (i.size > acc.size) i
          else acc
        }
        helper(tail.head, tail.tail, if (curr.size > largest.size) curr else largest)
      }
    }

    indexSetToString(helper(boxSets.head, boxSets.tail, Set.empty[(Char, Int)]))
  }

  def getChecksum(boxIDs: Iterable[String]) = {
    val mappedBoxIDs = boxIDs.map(id => new MappedOutWord(id))
    val has2 = mappedBoxIDs.filter(w => w.hasExactlyTwoOfALetter).toList
    val has3 = mappedBoxIDs.filter(w => w.hasExactlyThreeOfALetter).toList

    has2.length * has3.length
  }

  implicit def string2MappedOutWord(s: String): MappedOutWord = new MappedOutWord(s)

  def getInput = {
    Source.fromResource("input2.txt").getLines().toList

  }
}

class MappedOutWord(word: String) {

  val characterMap = word.toList.groupBy(identity).map(g => (g._1, g._2.length))

  def hasExactlyXOfALetter(x: Int) = characterMap.values.foldLeft(false)((b, n) => b || n == x)
  def hasExactlyTwoOfALetter = hasExactlyXOfALetter(2)
  def hasExactlyThreeOfALetter = hasExactlyXOfALetter(3)

}

