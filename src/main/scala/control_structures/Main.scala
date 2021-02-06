package control_structures

import scala.io.Source

final case class ErrorMessage(value: String) {
  def render(): String = s"Error: $value"
}

sealed trait Command {
  def calculateResult(): Either[ErrorMessage, Command.Result]
  def renderResult(result: Command.Result): String
}

object Command {
  type Arg = Double
  type Args = List[Arg]
  type Result = Double

  val AVERAGE = "average"
  val DIVIDE = "divide"
  val MAX = "max"
  val MIN = "min"
  val SUM = "sum"

  def parseCommand(raw: String): Either[ErrorMessage, Command] = {
    val tokens = raw.trim().split(' ').filter(_ != "").toList

    tokens match {
      case Command.AVERAGE :: args => parseCommandAverage(args)
      case Command.DIVIDE :: args  => parseCommandDivide(args)
      case Command.MAX :: args     => parseCommandMax(args)
      case Command.MIN :: args     => parseCommandMin(args)
      case Command.SUM :: args     => parseCommandSum(args)
      case command :: _            => Left(ErrorMessage(s"unknown command: $command"))
      case Nil                     => Left(ErrorMessage("empty input"))
    }
  }

  def parseCommandArgs(
      args: List[String]
  ): Either[ErrorMessage, Command.Args] = {
      val acc: Either[ErrorMessage, Command.Args] = Right(List[Double]())

      args
        .foldLeft(acc)((acc, x) => {
          acc.flatMap(nums =>
            x.toDoubleOption match {
              case Some(num) => Right(num :: nums)
              case None      => Left(ErrorMessage(s"invalid number: $x"))
            }
          )
        })
        .map(_.reverse)
  }

  def parseCommandDivide(
      args: List[String]
  ): Either[ErrorMessage, Command.Divide] = {
    parseCommandArgs(args).flatMap(args =>
      args match {
        case arg1 :: arg2 :: Nil => Right(Command.Divide(arg1, arg2))
        case _                   => Left(ErrorMessage("command DIVIDE accepts exactly 2 args"))
      }
    )
  }

  def parseCommandAverage(
      args: List[String]
  ): Either[ErrorMessage, Command.Average] = {
    parseCommandArgs(args).flatMap(args =>
      args match {
        case _ :: _ => Right(Command.Average(args))
        case _      => Left(ErrorMessage("command AVERAGE accepts at least 1 arg"))
      }
    )
  }

  def parseCommandSum(args: List[String]): Either[ErrorMessage, Command.Sum] = {
    parseCommandArgs(args).flatMap(args =>
      args match {
        case _ :: _ => Right(Command.Sum(args))
        case _      => Left(ErrorMessage("command SUM accepts at least 1 arg"))
      }
    )
  }

  def parseCommandMin(args: List[String]): Either[ErrorMessage, Command.Min] = {
    parseCommandArgs(args).flatMap(args =>
      args match {
        case _ :: _ => Right(Command.Min(args))
        case _      => Left(ErrorMessage("command MIN accepts at least 1 arg"))
      }
    )
  }

  def parseCommandMax(args: List[String]): Either[ErrorMessage, Command.Max] = {
    parseCommandArgs(args).flatMap(args =>
      args match {
        case _ :: _ => Right(Command.Max(args))
        case _      => Left(ErrorMessage("command MAX accepts at least 1 arg"))
      }
    )
  }

  final case class Divide(dividend: Arg, divisor: Arg) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      divisor match {
        case 0       => Left(ErrorMessage("division by 0"))
        case divisor => Right(dividend / divisor)
      }

    override def renderResult(result: Result): String =
      s"$dividend divided by $divisor is $result"
  }

  final case class Sum(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.sum)

    override def renderResult(result: Result): String =
      s"the sum of ${args.mkString(" ")} is $result"
  }

  final case class Average(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.sum / args.length)

    override def renderResult(result: Result): String =
      s"the average of ${args.mkString(" ")} is $result"
  }

  final case class Min(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.reduce(_ min _))

    override def renderResult(result: Result): String =
      s"the minimum of ${args.mkString(" ")} is $result"
  }

  final case class Max(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.reduce(_ max _))

    override def renderResult(result: Result): String =
      s"the maximum of ${args.mkString(" ")} is $result"
  }
}

object Main {
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  def process(command: String): String = {
    Command
      .parseCommand(command)
      .flatMap(command =>
        command.calculateResult().map(result => command.renderResult(result))
      )
      .fold(_.render, identity)
  }
}
