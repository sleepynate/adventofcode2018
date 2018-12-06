package adventofcode

import scala.io.Source

object Day5 {
  def shouldReactLetters(s: String): Boolean = {
    val fst = s charAt 0
    val snd = s charAt 1
    shouldReactLetters(fst, snd)
  }

  def shouldReactLetters(fst: Char, snd: Char): Boolean = {
    if (fst.isLower && snd.isUpper)
      fst == snd.toLower
    else if (snd.isLower && fst.isUpper)
      fst.toLower == snd
    else
      false
  }

  def reactString(s: String):String = {
    val newS = s.foldLeft(List.empty[Char]) { (s, c) =>
      if(s.isEmpty)
        c :: s
      else if(shouldReactLetters(s.head, c))
        s.tail
      else
        c :: s
    }.reverse.mkString
    if(newS == s) s else reactString(newS)
  }

  def findShortestPolymer(input: String):(Char, Int) = {
    val letters = input.map { _.toLower }.toSet
    val lettersRemovedInputs = letters.map { l => l -> input.filterNot(i => i.toLower == l || i.toUpper == l) }.toMap
    val reducedPolymers = lettersRemovedInputs.mapValues { i => reactString(i) }
    val lengths = reducedPolymers.mapValues(_.length)
    lengths.minBy{ _._2 }
  }

  def getInput: String = {
    Source.fromResource("input5.txt").mkString
  }

}
