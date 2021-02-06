package control_structures

import org.scalatest._
import flatspec._
import matchers._

class CommandParsingSpec extends AnyFlatSpec with should.Matchers {
  "Command parsing" should "tolerate empty input" in {
    val output = Command.parseCommand("")
    val expectedOutput = "empty input"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "tolerate unknown commands" in {
    val unknownCommand = "unknown"
    val output = Command.parseCommand(unknownCommand)
    val expectedOutput = s"unknown command: $unknownCommand"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "tolerate invalid numeric arguments" in {
    val invalidArg = "invalid"
    val commandArgs = List("1.0", "2", invalidArg)
    val output = Command.parseCommandArgs(commandArgs)
    val expectedOutput = s"invalid command arg: $invalidArg"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "successfully parse valid command args" in {
    val commandArgs = List("1.0", "2", "-1.3")
    val parsedArgs = Command.parseCommandArgs(commandArgs)
    val expectedArgs = List(1.0, 2.0, -1.3)

    parsedArgs should be(Right(expectedArgs))
  }
}

class DivideCommandSpec extends AnyFlatSpec with should.Matchers {
  "DIVIDE command" should "successfully parse with 2" in {
    val parsedCommand = Command.parseCommand("divide -3.0 2")
    val expectedCommand = Command.Divide(-3.0, 2)

    parsedCommand should be(Right(expectedCommand))
  }

  it should "tolerate parsing without args" in {
    val output = Command.parseCommand("divide")
    val expectedOutput = "command DIVIDE accepts exactly 2 args"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "tolerate parsing with 1 args" in {
    val output = Command.parseCommand("divide 2.0")
    val expectedOutput = "command DIVIDE accepts exactly 2 args"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "tolerate parsing with more than 2 args" in {
    val output = Command.parseCommand("divide 2.0 4.0 -1.0")
    val expectedOutput = "command DIVIDE accepts exactly 2 args"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "tolerate division by sero" in {
    val output = Command.parseCommand("divide 2.0 0").flatMap(_.calculateResult)
    val expectedOutput = "division by zero"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "calculate reuslt successfully" in {
    val result =
      Command.parseCommand("divide 2.0 -4").flatMap(_.calculateResult)
    val expectedResult = -0.5

    result should be(Right(expectedResult))
  }

  it should "render reuslt successfully" in {
    val output =
      Command
        .parseCommand("divide 2.0 -4")
        .flatMap(command => command.calculateResult.map(command.renderResult))
    val expectedOutput = "2.0 divided by -4.0 is -0.5"

    output should be(Right(expectedOutput))
  }
}

class SumCommandSpec extends AnyFlatSpec with should.Matchers {
  "SUM command" should "successfully parse with more than zero args" in {
    val parsedCommand = Command.parseCommand("sum -3.0 2")
    val expectedCommand = Command.Sum(List(-3.0, 2))

    parsedCommand should be(Right(expectedCommand))
  }

  it should "tolerate parsing without args" in {
    val output = Command.parseCommand("sum")
    val expectedOutput = "command SUM accepts at least 1 arg"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "calculate reuslt successfully" in {
    val result =
      Command.parseCommand("sum 1 2 3").flatMap(_.calculateResult)
    val expectedResult = 6

    result should be(Right(expectedResult))
  }

  it should "render reuslt successfully" in {
    val output =
      Command
        .parseCommand("sum 1 2 3")
        .flatMap(command => command.calculateResult.map(command.renderResult))
    val expectedOutput = "the sum of 1.0 2.0 3.0 is 6.0"

    output should be(Right(expectedOutput))
  }
}

class AverageCommandSpec extends AnyFlatSpec with should.Matchers {
  "AVERAGE command" should "successfully parse with more than zero args" in {
    val parsedCommand = Command.parseCommand("average -3.0 2")
    val expectedCommand = Command.Average(List(-3.0, 2))

    parsedCommand should be(Right(expectedCommand))
  }

  it should "tolerate parsing without args" in {
    val output = Command.parseCommand("average")
    val expectedOutput = "command AVERAGE accepts at least 1 arg"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "calculate reuslt successfully" in {
    val result =
      Command.parseCommand("average 1 2 3").flatMap(_.calculateResult)
    val expectedResult = 2

    result should be(Right(expectedResult))
  }

  it should "render reuslt successfully" in {
    val output =
      Command
        .parseCommand("average 1 2 3")
        .flatMap(command => command.calculateResult.map(command.renderResult))
    val expectedOutput = "the average of 1.0 2.0 3.0 is 2.0"

    output should be(Right(expectedOutput))
  }
}

class MinCommandSpec extends AnyFlatSpec with should.Matchers {
  "MIN command" should "successfully parse with more than zero args" in {
    val parsedCommand = Command.parseCommand("min -3.0 2")
    val expectedCommand = Command.Min(List(-3.0, 2))

    parsedCommand should be(Right(expectedCommand))
  }

  it should "tolerate parsing without args" in {
    val output = Command.parseCommand("min")
    val expectedOutput = "command MIN accepts at least 1 arg"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "calculate reuslt successfully" in {
    val result =
      Command.parseCommand("min 1 2 3").flatMap(_.calculateResult)
    val expectedResult = 1

    result should be(Right(expectedResult))
  }

  it should "render reuslt successfully" in {
    val output =
      Command
        .parseCommand("min 1 2 3")
        .flatMap(command => command.calculateResult.map(command.renderResult))
    val expectedOutput = "the minimum of 1.0 2.0 3.0 is 1.0"

    output should be(Right(expectedOutput))
  }
}

class MaxCommandSpec extends AnyFlatSpec with should.Matchers {
  "MAX command" should "successfully parse with more than zero args" in {
    val parsedCommand = Command.parseCommand("max -3.0 2")
    val expectedCommand = Command.Max(List(-3.0, 2))

    parsedCommand should be(Right(expectedCommand))
  }

  it should "tolerate parsing without args" in {
    val output = Command.parseCommand("max")
    val expectedOutput = "command MAX accepts at least 1 arg"

    output should be(Left(ErrorMessage(expectedOutput)))
  }

  it should "calculate reuslt successfully" in {
    val result =
      Command.parseCommand("max 1 2 3").flatMap(_.calculateResult)
    val expectedResult = 3

    result should be(Right(expectedResult))
  }

  it should "render reuslt successfully" in {
    val output =
      Command
        .parseCommand("max 1 2 3")
        .flatMap(command => command.calculateResult.map(command.renderResult))
    val expectedOutput = "the maximum of 1.0 2.0 3.0 is 3.0"

    output should be(Right(expectedOutput))
  }
}
