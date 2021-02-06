package control_structures

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
      case Command.AVERAGE :: args => Command.Average.parse(args)
      case Command.DIVIDE :: args  => Command.Divide.parse(args)
      case Command.MAX :: args     => Command.Max.parse(args)
      case Command.MIN :: args     => Command.Min.parse(args)
      case Command.SUM :: args     => Command.Sum.parse(args)
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
        acc.flatMap(args =>
          x.toDoubleOption match {
            case Some(arg) => Right(arg :: args)
            case None      => Left(ErrorMessage(s"invalid command arg: $x"))
          }
        )
      })
      .map(_.reverse)
  }

  final case class Divide(dividend: Arg, divisor: Arg) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      divisor match {
        case 0       => Left(ErrorMessage("division by zero"))
        case divisor => Right(dividend / divisor)
      }

    override def renderResult(result: Result): String =
      s"$dividend divided by $divisor is $result"
  }
  final object Divide {
    def parse(args: List[String]): Either[ErrorMessage, Command.Divide] = {
      parseCommandArgs(args).flatMap(args =>
        args match {
          case arg1 :: arg2 :: Nil => Right(Command.Divide(arg1, arg2))
          case _                   => Left(ErrorMessage("command DIVIDE accepts exactly 2 args"))
        }
      )
    }
  }

  final case class Sum(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.sum)

    override def renderResult(result: Result): String =
      s"the sum of ${args.mkString(" ")} is $result"
  }
  final object Sum {
    def parse(args: List[String]): Either[ErrorMessage, Command.Sum] = {
      parseCommandArgs(args).flatMap(args =>
        args match {
          case _ :: _ => Right(Command.Sum(args))
          case _      => Left(ErrorMessage("command SUM accepts at least 1 arg"))
        }
      )
    }
  }

  final case class Average(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.sum / args.length)

    override def renderResult(result: Result): String =
      s"the average of ${args.mkString(" ")} is $result"
  }
  final object Average {
    def parse(args: List[String]): Either[ErrorMessage, Command.Average] = {
      parseCommandArgs(args).flatMap(args =>
        args match {
          case _ :: _ => Right(Command.Average(args))
          case _      => Left(ErrorMessage("command AVERAGE accepts at least 1 arg"))
        }
      )
    }
  }

  final case class Min(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.reduce(_ min _))

    override def renderResult(result: Result): String =
      s"the minimum of ${args.mkString(" ")} is $result"
  }
  final object Min {
    def parse(args: List[String]): Either[ErrorMessage, Command.Min] = {
      parseCommandArgs(args).flatMap(args =>
        args match {
          case _ :: _ => Right(Command.Min(args))
          case _      => Left(ErrorMessage("command MIN accepts at least 1 arg"))
        }
      )
    }
  }

  final case class Max(args: Args) extends Command {
    override def calculateResult(): Either[ErrorMessage, Result] =
      Right(args.reduce(_ max _))

    override def renderResult(result: Result): String =
      s"the maximum of ${args.mkString(" ")} is $result"
  }
  final object Max {
    def parse(args: List[String]): Either[ErrorMessage, Command.Max] = {
      parseCommandArgs(args).flatMap(args =>
        args match {
          case _ :: _ => Right(Command.Max(args))
          case _      => Left(ErrorMessage("command MAX accepts at least 1 arg"))
        }
      )
    }
  }
}
