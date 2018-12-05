package me.adventofcode.y2018.d4

import java.time.{LocalDate, LocalDateTime}

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex

object ReposeRecord {

  def parseAndBuildStats(logRecords: List[String]): List[DayStat] =
    buildStats(
      logRecords.sorted.flatMap(LogRecord.parse(_).toOption)
    )

  // todo: verify input?
  def buildStats(logRecords: List[LogRecord]): List[DayStat] =
    logRecords
      .groupBy {
        case LogRecord(timestamp, BeginsShift(_)) if timestamp.getHour >= 23 => timestamp.plusDays(1).toLocalDate
        case LogRecord(timestamp, _) => timestamp.toLocalDate
      }
      .toList
      .map(buildDayStat _ tupled)
      .sortWith((first, second) => first.date.compareTo(second.date) < 0)

  // todo: exhaustive pattern match
  private def buildDayStat(date: LocalDate, dayRecords: List[LogRecord]): DayStat = {
    val (stat, _) = dayRecords match {
      case LogRecord(_, BeginsShift(guardId)) :: tailRecords =>
        tailRecords.foldLeft((DayStat(date, guardId, Set.empty), Option.empty[Int])) {
          case ((dayStat, sleepingSince), currentRecord) =>
            currentRecord match {
              case LogRecord(timestamp, FallsAsleep) => (dayStat, sleepingSince.orElse(Some(timestamp.getMinute)))
              case LogRecord(timestamp, WakesUp) =>
                val updatedStat = sleepingSince.map(dayStat.addSleepingTime(_, timestamp.getMinute)).getOrElse(dayStat)
                (updatedStat, None)
              case _ => (dayStat, sleepingSince)
            }
        }
    }

    stat
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

  val BeginsShiftRegex: Regex = s"Guard #(\\d{2}) begins shift".r

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



