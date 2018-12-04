package me.adventofcode.y2018.d4

import java.time.LocalDateTime

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex


object ReposeRecord {

}

case class LogRecord(timestamp: LocalDateTime, eventType: EventType)

object LogRecord {

  val LogTimestampPattern = "yyyy-MM-dd HH:mm"

  val LogRecordRegex: Regex = s"\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] (.*)".r

  def parseLogTimestamp: String => Either[String, LocalDateTime] = parseTimeStamp(LogTimestampPattern)

  def parse(value: String): Either[String, LogRecord] = value match {
    case LogRecordRegex(timestampStr, message) =>
      for {
        timestamp <- parseLogTimestamp(timestampStr)
        event     <- EventType.parse(message)
      } yield LogRecord(timestamp, event)
    case _ => Left(s"Unable to parse record: $value")
  }

}

sealed trait EventType

object EventType {

  val BeginsShiftRegex: Regex = s"Guard #(\\d{2}) begins shift".r

  def parse(message: String): Either[String, EventType] = message match {
    case "falls asleep"       => Right(FallsAsleep)
    case "wakes up"           => Right(WakesUp)
    case BeginsShiftRegex(id) => id.parseInt.map(BeginsShift)
    case other                => Left(s"Unsupported event: $other")
  }

}

case class BeginsShift(guardId: Int) extends EventType

case object FallsAsleep extends EventType

case object WakesUp extends EventType



