import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def markup: Parser[Body] = body ^^ { case b => Body(b) }
  def body: Parser[List[Element]] = rep(header | para | blankline) ^^ {
    case children => children.filter(_.isInstanceOf[Element]).map(_.asInstanceOf[Element])
  }
  def header: Parser[Header] = rep("*") ~ " " ~ text ^^ { case h ~ s ~ t => Header(h.size, t) }
  def para: Parser[Para] = text ~ newline ^^ { case c ~ n => Para(List(c)) }
  def text: Parser[Text] = rep1(contentChar) ^^ { case chars => Text(chars.mkString.trim) }
  def blankline: Parser[String] = newline ^^ { case n => "[blank]" }
  def line: Parser[String] = text ~ newline ^^ { case c ~ n1 => c.toString }
  def contentChar: Parser[Any] = """[a-z]""".r | " "
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}
