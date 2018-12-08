package adventofcode

import java.time.Duration

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable

class Day4Suite extends FunSuite {

  import Day4._

  val testLogData = """[1518-11-01 00:00] Guard #10 begins shift
                      |[1518-11-01 00:05] falls asleep
                      |[1518-11-01 00:25] wakes up
                      |[1518-11-01 00:30] falls asleep
                      |[1518-11-01 00:55] wakes up
                      |[1518-11-01 23:58] Guard #99 begins shift
                      |[1518-11-02 00:40] falls asleep
                      |[1518-11-02 00:50] wakes up
                      |[1518-11-03 00:05] Guard #10 begins shift
                      |[1518-11-03 00:24] falls asleep
                      |[1518-11-03 00:29] wakes up
                      |[1518-11-04 00:02] Guard #99 begins shift
                      |[1518-11-04 00:36] falls asleep
                      |[1518-11-04 00:46] wakes up
                      |[1518-11-05 00:03] Guard #99 begins shift
                      |[1518-11-05 00:45] falls asleep
                      |[1518-11-05 00:55] wakes up""".stripMargin

  test("Can parse log entry type") {
    LogEntry.parse("[1518-10-28 00:52] wakes up") shouldBe a [WakeUp]
    LogEntry.parse("[1518-06-11 00:31] falls asleep") shouldBe a [FallAsleep]

    val entry = LogEntry.parse("[1518-04-18 00:01] Guard #401 begins shift")
    entry shouldBe a [BeginShift]
    entry.asInstanceOf[BeginShift].guard shouldBe 401
  }

  test("Guard 10 has 3 durations of sleeping") {
    val sleepyTimes = guardsAsleepMinutes(sortedLogEntries(testLogData.lines.toList))
    sleepyTimes(10).length shouldBe 50
    sleepyTimes(99).length shouldBe 30

    val (sleepiestGuard, timesSlept) = sleepyTimes.maxBy { case (g, mins) => mins.length }
    val bestMinute = timesSlept.groupBy(identity)
                               .mapValues(_.size)
                               .maxBy{ case (_, amt) => amt }._1
      //.maxBy(_._2)._1
    bestMinute shouldBe 24
  }

  ignore("Can get part 1 answer") {
    val sleepyTimes = guardsAsleepMinutes(sortedLogEntries(getInput))
    val (sleepiestGuard, timesSlept) = sleepyTimes.maxBy { case (_, mins) => mins.length }
    val bestMinute = timesSlept.groupBy(identity)
      .mapValues(_.size)
      .maxBy{ case (_, amt) => amt }._1
    sleepiestGuard shouldBe 2663
    bestMinute shouldBe 45
  }

  test("Guard 99 slept minute 45 the most") {
    val sleepyTimes = guardsAsleepMinutes(sortedLogEntries(testLogData.lines.toList))
    //                         minute slept----v    v----- number of occurrences
    val guardToMostCommonMinute: Map[GuardID, (Int, Int)] = sleepyTimes.map {
      case (guardId, minutes) => (guardId, minutes.groupBy(identity).mapValues(_.size).maxBy(_._2))
    }

    val sleepiestGuard = guardToMostCommonMinute.maxBy {
      case (guardId, (minutes, occurrence)) => occurrence
    }
    sleepiestGuard._1 shouldBe 99
    sleepiestGuard._2._1 shouldBe 45
  }

  ignore("Solve Day 4 problem 2") {
    val sleepyTimes = guardsAsleepMinutes(sortedLogEntries(getInput))
    //                         minute slept----v    v----- number of occurrences
    val guardToMostCommonMinute: Map[GuardID, (Int, Int)] = sleepyTimes.map {
      case (guardId, minutes) => (guardId, minutes.groupBy(identity).mapValues(_.size).maxBy(_._2))
    }

    val sleepiestGuard = guardToMostCommonMinute.maxBy {
      case (guardId, (minutes, occurrence)) => occurrence
    }
    sleepiestGuard._1 shouldBe 509
    sleepiestGuard._2._1 shouldBe 25
  }
}
