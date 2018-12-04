package me.adventofcode.y2018.d4

import java.time.LocalDate

import me.adventofcode.y2018.d4.ReposeRecordSpec.TestLog
import org.scalatest.{FunSuite, Inside, Matchers}

class ReposeRecordSpec extends FunSuite with Matchers with Inside {

  test("parse log timestamp") {
    inside(LogRecord.parseLogTimestamp("1518-11-02 00:40")) {
      case Right(timestamp) =>
        timestamp.getHour shouldEqual 0
        timestamp.getMinute shouldEqual 40
        timestamp.getDayOfMonth shouldEqual 2
        timestamp.getMonthValue shouldEqual 11
        timestamp.getYear shouldEqual 1518
    }
  }

  test("parse log record") {
    val fallsAsleepLog = "[1518-11-02 00:40] falls asleep"
    inside(LogRecord.parseLogTimestamp("1518-11-02 00:40")) {
      case Right(timestamp) =>
        LogRecord.parse(fallsAsleepLog) shouldBe Right(LogRecord(timestamp, FallsAsleep))
    }

    val beginsShiftLog = "[1518-11-05 00:03] Guard #99 begins shift"
    inside(LogRecord.parseLogTimestamp("1518-11-05 00:03")) {
      case Right(timestamp) =>
        LogRecord.parse(beginsShiftLog) shouldBe Right(LogRecord(timestamp, BeginsShift(99)))
    }

    val WakesUpLog = "[1518-11-02 00:50] wakes up"
    inside(LogRecord.parseLogTimestamp("1518-11-02 00:50")) {
      case Right(timestamp) =>
        LogRecord.parse(WakesUpLog) shouldBe Right(LogRecord(timestamp, WakesUp))
    }

  }

  test("build daily stats from unordered log events") {

    val records = TestLog
      .split('\n')
      .flatMap(line => LogRecord.parse(line).toOption)
      .toList

    ReposeRecord.buildStats(records) shouldBe List(
      DayStat(
        LocalDate.parse("1518-11-01"),
        10,
        ((5 to 24) ++ (30 to 54)).toSet
      ),
      DayStat(LocalDate.parse("1518-11-02"), 99, (40 to 49).toSet),
      DayStat(LocalDate.parse("1518-11-03"), 10, (5 to 24).toSet),
      DayStat(LocalDate.parse("1518-11-04"), 99, (36 to 45).toSet),
      DayStat(LocalDate.parse("1518-11-05"), 99, (45 to 54).toSet)
    )

  }

}

object ReposeRecordSpec {

  val TestLog: String =
    """
      |[1518-11-02 00:40] falls asleep
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:05] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-01 00:55] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:29] wakes up
      |[1518-11-05 00:55] wakes up
      |[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-04 00:46] wakes up
      |[1518-11-03 00:24] falls asleep
      |[1518-11-05 00:45] falls asleep
    """.stripMargin

}
