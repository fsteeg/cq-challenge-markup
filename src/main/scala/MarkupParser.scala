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
  def body: Parser[List[Element]] = rep(verb | block | header | para | blankline) ^^ {
    case children => children.filter(_.isInstanceOf[Element]).map(_.asInstanceOf[Element])
  }
  def verb:Parser[Verb] = repN(3, " ")~rawText ^^ { case space~content => Verb(content) }
  def block:Parser[Block] = repN(2, " ")~rep(para) ^^ { case space~content => Block(content) }
  def header: Parser[Header] = rep("*") ~ " " ~ text ^^ { case h ~ s ~ t => Header(h.size, t) }
  def para: Parser[Para] = text ~ opt(blankline) ^^ { case c ~ n => Para(List(c)) }
  def text: Parser[Text] = rep1(line) ^^ { case chars => Text(chars.mkString(" ").trim) }
  def blankline: Parser[String] = newline ^^ { case n => "[blank]" }
  def line: Parser[String] = rep1(contentChar) ~ newline ^^ { case c ~ n1 => c.mkString }
  def contentChar: Parser[Any] = """[\w\.,;'-]""".r | " "
  def rawText: Parser[Text] = rep1(raw) ^^ { case text => Text(text.mkString.trim) }
  def raw: Parser[Any] = """.""".r | whiteSpace
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}
