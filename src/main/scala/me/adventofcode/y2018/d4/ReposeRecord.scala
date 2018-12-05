package me.adventofcode.y2018.d4

import java.time.{LocalDate, LocalDateTime}

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex

class GuardStatistics(logRecords: List[String]) {

  lazy val dailyStats: List[DayStat] =
    logRecords
      .sorted
      .flatMap(LogRecord.parse(_).toOption)
      .groupBy {
        case LogRecord(timestamp, BeginsShift(_)) if timestamp.getHour == 23 => timestamp.plusDays(1).toLocalDate
        case LogRecord(timestamp, _) => timestamp.toLocalDate
      }
      .toList
      .map { case (date, records) => DayStat(date, records) }
      .sortWith((first, second) => first.date.compareTo(second.date) < 0)

  lazy val sleepyGuardAndMinute: (Int, Int) = {
    val statsByGuard = dailyStats.groupBy(_.guardId)

    val (mostSleepyGuard, _) = statsByGuard.maxBy(_._2.map(_.sleepMinutes.size).sum)

    val mostSleepyMinute = statsByGuard(mostSleepyGuard)
      .flatMap(_.sleepMinutes.toList)
      .groupBy(identity)
      .maxBy(_._2.size)
      ._1

    (mostSleepyGuard, mostSleepyMinute)
  }

}

case class GuardStat(guardId: Int, sleepingMinutes: Map[Int, Int])

object DayStat {

  // todo: exhaustive pattern match
  def apply(date: LocalDate, dayRecords: List[LogRecord]): DayStat = {
    val (resultStat, _) = dayRecords match {
      case LogRecord(_, BeginsShift(guardId)) :: tailRecords =>

        val initStat = DayStat(date, guardId, Set.empty)
        val initAsleepSince = Option.empty[Int]

        tailRecords.foldLeft((initStat, initAsleepSince)) {
          case ((dayStat, None), LogRecord(timestamp, FallsAsleep))          => (dayStat, Some(timestamp.getMinute))
          case ((dayStat, Some(asleepSince)), LogRecord(timestamp, WakesUp)) => (dayStat.addSleepingTime(asleepSince, timestamp.getMinute), None)
          case (result, _)                                                   => result
        }
    }

    resultStat
  }

}

case class DayStat(date: LocalDate, guardId: Int, sleepMinutes: Set[Int]) {

  def addSleepingTime(firstMinute: Int, lastMinute: Int): DayStat =
    DayStat(date, guardId, sleepMinutes ++ (firstMinute until lastMinute).toSet)

}

case class LogRecord(timestamp: LocalDateTime, eventType: GuardEvent)

object LogRecord {

  val LogTimestampPattern = "yyyy-MM-dd HH:mm"

  val LogRecordRegex: Regex = s"\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] (.*)".r

  def parseLogTimestamp: String => Either[String, LocalDateTime] = parseTimeStamp(LogTimestampPattern)

  def parse(value: String): Either[String, LogRecord] = value match {
    case LogRecordRegex(timestampStr, message) =>
      for {
        timestamp <- parseLogTimestamp(timestampStr)
        event <- GuardEvent.parse(message)
      } yield LogRecord(timestamp, event)
    case _ => Left(s"Unable to parse record: $value")
  }

}

sealed trait GuardEvent

object GuardEvent {

  val BeginsShiftRegex: Regex = s"Guard #(\\d+) begins shift".r

  def parse(message: String): Either[String, GuardEvent] = message match {
    case "falls asleep"       => Right(FallsAsleep)
    case "wakes up"           => Right(WakesUp)
    case BeginsShiftRegex(id) => id.parseInt.map(BeginsShift)
    case other                => Left(s"Unsupported event: $other")
  }

}

case class BeginsShift(guardId: Int) extends GuardEvent

case object FallsAsleep extends GuardEvent

case object WakesUp extends GuardEvent



