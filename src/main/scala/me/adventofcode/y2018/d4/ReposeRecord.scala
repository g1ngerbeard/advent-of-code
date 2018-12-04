package me.adventofcode.y2018.d4

import java.time.{LocalDate, LocalDateTime}

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex

object ReposeRecord {

  // todo: veriry input?
  def buildStats(logRecords: List[LogRecord]): List[DayStat] = {
    logRecords
      .sortWith((first, second) => first.timestamp.compareTo(second.timestamp) < 0)
      .groupBy(_.timestamp.toLocalDate)
      .toList
      .map(buildDayStat _ tupled)
  }

  private def buildDayStat(date: LocalDate, dayRecords: List[LogRecord]): DayStat = {

    val (resultStat, _) = dayRecords match {
      case (first @ LogRecord(_, BeginsShift(guardId))) :: restRecords =>
          restRecords.foldLeft((DayStat(date, guardId, Set.empty), first)){
            case ((stat, lastRecord), record) => record.eventType match {
              case FallsAsleep => (stat, record)
              case WakesUp =>
                (stat.addSleepingTime(lastRecord.timestamp.getMinute, record.timestamp.getMinute), record)
            }
          }
    }

    resultStat
  }

}

case class DayStat(date: LocalDate, guardId: Int, sleepMinutes: Set[Int]) {

  // todo: to or until?
  def addSleepingTime(firstMinute: Int, lastMinute: Int): DayStat =
    DayStat(date, guardId, sleepMinutes ++ (firstMinute to lastMinute).toSet)


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
        event     <- GuardEvent.parse(message)
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



