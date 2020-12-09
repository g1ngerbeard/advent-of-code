package me.adventofcode.y2020

import me.adventofcode.util.parsers._
import me.adventofcode.util.extensions._
import me.adventofcode.y2020.PolicyType.{Boundary, OnePosition}

import scala.util.matching.Regex

object Day2 {

  def part1(inputs: List[String]): Int =
    inputs
      .map(InputLine.parse(PolicyType.Boundary, _))
      .count(_.isRight)

  def part2(inputs: List[String]): Int =
    inputs
      .map(InputLine.parse(PolicyType.OnePosition, _))
      .count(_.isRight)

}

sealed trait Problem

case object InvalidPassword extends Problem

case class InvalidPolicy(reason: String) extends Problem

case class InvalidInputLine(reason: String) extends Problem

sealed trait PolicyType

object PolicyType {

  case object Boundary extends PolicyType

  case object OnePosition extends PolicyType

}

case class PasswordPolicy private (a: Int, b: Int, char: Char, policyType: PolicyType)

object PasswordPolicy {

  def make(a: Int, b: Int, char: Char, policyType: PolicyType): Either[Problem, PasswordPolicy] =
    Either.cond(
      policyType != Boundary || a <= b,
      PasswordPolicy(a, b, char, policyType),
      InvalidPolicy("Invalid policy bounds")
    )

}

case class Password private (value: String)

object Password {

  def parse(policy: PasswordPolicy, value: String): Either[Problem, Password] =
    policy.policyType match {
      case Boundary =>
        val count = value.count(_ == policy.char)

        Either.cond(
          count >= policy.a && count <= policy.b,
          Password(value),
          InvalidPassword
        )

      case OnePosition =>
        val ca = value.charAt(policy.a - 1)
        val cb = value.charAt(policy.b - 1)

        Either.cond(
          List(ca, cb).count(_ == policy.char) == 1,
          Password(value),
          InvalidPassword
        )
    }

}

case class InputLine private (password: Password, policy: PasswordPolicy)

object InputLine {

  val LineRegex: Regex = "(\\d+)-(\\d+) (\\w): (\\w+)".r

  def parse(policyType: PolicyType, line: String): Either[Problem, InputLine] =
    line match {
      case LineRegex(rawLBound, rawUBound, rawChar, rawPassword) =>
        for {
          a <- rawLBound.parseInt.leftMap(InvalidInputLine)
          b <- rawUBound.parseInt.leftMap(InvalidInputLine)
          char <- rawChar.headOption.toRight(InvalidInputLine("invalid char"))
          policy <- PasswordPolicy.make(a, b, char, policyType)
          password <- Password.parse(policy, rawPassword)
        } yield InputLine(password, policy)

      case _ => InvalidInputLine("Unable to parse input line").asLeft
    }

}
