package me.adventofcode.y2019

object Day2 {

  sealed trait Problem

  case class InvalidCode(code: Int, pos: Int) extends Problem

  case class OutOfBounds(pos: Int) extends Problem

  type ProgramR[T] = Either[Problem, T]

  def runIntcode(input: Vector[Int]): ProgramR[Vector[Int]] = {

    // not stack safe ;(
    def loop(pos: Int, cmdArr: Vector[Int]): ProgramR[Vector[Int]] =
      getCode(pos, cmdArr)
        .flatMap {
          case 99 => Right(cmdArr)
          case code =>
            for {
              nextArr <- handleOpCode(pos, code, cmdArr)
              result  <- loop(pos + 4, nextArr)
            } yield result
        }

    loop(0, input)
  }

  private def handleOpCode(pos: Int, code: Int, cmdArr: Vector[Int]): ProgramR[Vector[Int]] = code match {
    case 1 => applyOperation(pos, cmdArr)(_ + _)
    case 2 => applyOperation(pos, cmdArr)(_ * _)
    case code => Left(InvalidCode(code, pos))
  }

  private def getCode(pos: Int, cmdArr: Vector[Int]): ProgramR[Int] =
    if (pos < cmdArr.size) {
      Right(cmdArr(pos))
    } else {
      Left(OutOfBounds(pos))
    }

  private def applyOperation(pos: Int, cmdArr: Vector[Int])(op: (Int, Int) => Int): ProgramR[Vector[Int]] = {
    for {
      inputAIdx <- getCode(pos + 1, cmdArr)
      inputA <- getCode(inputAIdx, cmdArr)
      inputBIdx <- getCode(pos + 2, cmdArr)
      inputB <- getCode(inputBIdx, cmdArr)
      outputIdx <- getCode(pos + 3, cmdArr)
      _ <- getCode(outputIdx, cmdArr)
    } yield cmdArr.updated(outputIdx, op(inputA, inputB))
  }

}
