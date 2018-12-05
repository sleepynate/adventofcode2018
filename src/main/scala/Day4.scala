package adventofcode

import java.time.{Duration, LocalDate, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day4 {
  type GuardID = Int

  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))

  abstract class LogEntry

  object LogEntry {
    val parserFmt: Regex = raw"\[(.*)\] (.*)".r
    val beginShiftFmt: Regex = raw"Guard #(\d+) begins shift".r

    def parse(s: String): LogEntry = {
      s match {
        case parserFmt(date, msg) =>
          val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
          val minutes = LocalDateTime.parse(date, dtf).getMinute
          msg match {
            case "wakes up" => WakeUp(minutes)
            case "falls asleep" => FallAsleep(minutes)
            case beginShiftFmt(guard) => BeginShift(guard.toInt)
          }
      }
    }
  }

  case class BeginShift(guard: Int) extends LogEntry
  case class FallAsleep(minutes: Int) extends LogEntry
  case class WakeUp(minutes: Int) extends LogEntry

  def getInput: List[String] = Source.fromResource("input4.txt").getLines().toList

  def sortedLogEntries(entries: List[String]): List[LogEntry] = entries.sorted.map(LogEntry.parse)

  def guardsAsleepMinutes(entries: List[LogEntry]): Map[GuardID, List[Int]] = {
    val asleepTimes = new mutable.HashMap[GuardID, List[Int]]

    var lastSleep: Int = 0
    var onDuty: GuardID = 0
    entries.foreach {
      case BeginShift(guard) => onDuty = guard
      case FallAsleep(mins) => lastSleep = mins
      case WakeUp(mins) =>
        val currentDurations = asleepTimes.getOrElse(onDuty, List.empty[Int])
        val duration = (lastSleep until mins).toList

        asleepTimes += onDuty -> (duration ++ currentDurations)
    }
    asleepTimes.toMap
  }

}

