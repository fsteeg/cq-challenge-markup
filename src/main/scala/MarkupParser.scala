import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers

class MarkupParser extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def markup: Parser[List[String]] = rep(line) ^^ { case lines => lines.map(_._1) }
  def line: Parser[(String, String)] = content ~ newline ^^ {case c ~ n => (c.toString, n.toString)}
  def content: Parser[Any] = opt(ident)
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}