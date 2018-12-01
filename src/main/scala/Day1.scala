package adventofcode

import scala.io.{BufferedSource, Source}

object Day1 {

  val accFunction = (acc: Int, line: String) =>
    line.head match {
      case '+' => acc + line.tail.trim.toInt
      case '-' => acc - line.tail.trim.toInt
    }


  def frequencyChange(start: Int, input: String): Int = {
    val splitInput = input.trim.split('\n')
    splitInput.foldLeft(start)(accFunction)
  }

  def frequencyChangeHist(start: Int, input: String): Int = {
    val splitInput = input.trim.split('\n')
    var idx:Int = 0
    var startFreq = 0
    var seenFreqs: Set[Int] = Set()
    while (!(seenFreqs.contains(startFreq))) {
      seenFreqs = seenFreqs + startFreq
      startFreq = accFunction(startFreq, splitInput(idx))
      idx += 1
      if (idx == splitInput.length) {
        idx = 0
      }
    }
    startFreq
  }

  def getInput = {
    Source.fromResource("input1.txt").mkString
  }
}
