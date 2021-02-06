package control_structures

import scala.io.Source

final case class ErrorMessage(value: String) {
  def render(): String = s"Error: $value"
}

object Main {
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  def process(command: String): String = {
    Command
      .parseCommand(command)
      .flatMap(command => command.calculateResult().map(command.renderResult))
      .fold(_.render, identity)
  }
}
