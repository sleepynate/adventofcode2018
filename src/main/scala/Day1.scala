package adventofcode

import scala.io.{BufferedSource, Source}

object Day1 {

  def frequencyChange(start: Int, input: String): Int = {
    val splitInput = input.split('\n')
    splitInput.foldLeft(start) { (acc, line) =>
      line.head match {
        case '+' => acc + line.tail.trim.toInt
        case '-' => acc - line.tail.trim.toInt
        case _ => acc
      }
    }
  }

  def getInput = {
    Source.fromResource("input1.txt").mkString
  }
}
