package adventofcode

import scala.io.Source
import scala.language.implicitConversions

object Day2 {
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

