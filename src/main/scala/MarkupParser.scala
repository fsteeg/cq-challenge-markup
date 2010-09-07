import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser extends MarkupLexer {
  def body: Parser[Body] = rep(list | pre | blockquote | h | p | blankline) ^^ {
    case children => Body(children.filter(_.isInstanceOf[Markup]).map(_.asInstanceOf[Markup]))
  }
  def list: Parser[Markup] = ol | ul
  def ol: Parser[Ol] = rep1(repN(2, " ") ~ "# " ~ li) ^^ {
    case list => Ol(list.map(_ match { case b ~ s ~ i => i }))
  }
  def ul: Parser[Ul] = rep1(repN(2, " ") ~ "- " ~ li) ^^ {
    case list => Ul(list.map(_ match { case b ~ s ~ i => i }))
  }
  def li: Parser[Li] = p ^^ { case p => Li(List(p)) }
  def pre: Parser[Pre] = repN(3, " ") ~ rawText ^^ { case space ~ content => Pre(content) }
  def blockquote: Parser[BlockQuote] = repN(2, " ") ~ rep(p) ^^ { case space ~ content => BlockQuote(content) }
  def h: Parser[H] = rep("*") ~ " " ~ text ^^ { case h ~ s ~ t => H(h.size, t) }
  def p: Parser[P] = text ~ opt(blankline) ^^ { case c ~ n => P(c) }
}

class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def text: Parser[String] = rep1(line) ^^ { case chars => chars.mkString(" ").trim }
  def blankline: Parser[String] = newline ^^ { case n => "[blank]" }
  def line: Parser[String] = rep1(contentChar) ~ newline ^^ { case c ~ n1 => c.mkString }
  def contentChar: Parser[Any] = """[\w\.,;'-]""".r | " "
  def rawText: Parser[String] = rep1(raw) ^^ { case text => text.mkString.trim }
  def raw: Parser[Any] = """.""".r | whiteSpace
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}
