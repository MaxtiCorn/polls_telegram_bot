package domain

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  val args = new ListBuffer[String]

  def parameter: Parser[String] =
    """(\(+.*?\))""".r ^^ { arg => {
      this.args += arg.substring(1, arg.length - 1)
      arg.substring(1, arg.length - 1)
    }
    }

  def command_name: Parser[String] = "/[a-z]+(_[a-z]+)?".r ^^ { arg => {
    this.args += arg
    arg.toString
  }
  }

  def command: Parser[String] = command_name ~ rep(parameter) ^^ {
    _.toString()
  }

  def parse(text: String): Seq[String] = {
    parse(command, text)
    args
  }
}
