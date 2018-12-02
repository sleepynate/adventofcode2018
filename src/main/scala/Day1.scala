package adventofcode

import scala.io.Source

object Day1 {

  val accFunction = (acc: Int, line: String) =>
    line.head match {
      case '+' => acc + line.tail.trim.toInt
      case '-' => acc - line.tail.trim.toInt
    }


  def calculateFrequencyChange(input: String): Int = {
    val splitInput = input.trim.split('\n')
    splitInput.foldLeft(0)(accFunction)
  }

  def findRecurringFrequency(input: String): Int = {
    val splitInput = input.trim.split('\n')
    val length = splitInput.length

    def helper(idx: Int, currentFreq: Int, seenFreqs: Set[Int]): Int = {
      if (seenFreqs.contains(currentFreq)) {
        currentFreq
      } else {
        val nextFreq = accFunction(currentFreq, splitInput(idx))
        if (idx + 1 == length)
          helper(0, nextFreq, seenFreqs + currentFreq)
        else
          helper(idx + 1, nextFreq, seenFreqs + currentFreq)
      }
    }

    helper(0, 0, Set())
  }

  def getInput = {
    Source.fromResource("input1.txt").mkString
  }
}
