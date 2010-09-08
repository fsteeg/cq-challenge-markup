import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser extends MarkupLexer {

  def body: Parser[Body] = rep(list | pre | blockquote | h | p) ^^ { case c => Body(c) }

  def list: Parser[Markup] = ol | ul
  def ol: Parser[Ol] = rep1(repN(2, " ") ~ "# " ~ li) ^^ {
    case list => Ol(list.map(_ match { case b ~ s ~ i => i }))
  }
  def ul: Parser[Ul] = rep1(repN(2, " ") ~ "- " ~ li) ^^ {
    case list => Ul(list.map(_ match { case b ~ s ~ i => i }))
  }
  def li: Parser[Li] = p ^^ { case p => Li(List(p)) }

  def pre: Parser[Pre] = repN(3, " ") ~ rawText ^^ { case s ~ c => Pre(c) }
  def h: Parser[H] = rep1("*") ~ " " ~ text ^^ { case h ~ s ~ t => H(h.size, t) }
  def p: Parser[P] = text ~ opt(newline) ^^ { case c ~ n => P(List(TextMarkup(c))) }

  def blockquote: Parser[BlockQuote] = block(2) ^^ {
    case b => { BlockQuote(parseAll(body, b).get.items) }
  }
  def block(n: Int): Parser[String] = rep1(repN(n, " ") ~ line ~ opt(newline)) ^^ {
    case text => text.map(_ match {
      case blanks ~ content ~ None => content
      case blanks ~ content ~ nl => content + "\n"
    }).mkString("\n") + "\n"
  }

}

class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def text: Parser[String] = rep1(line) ~ opt(newline) ^^ { case chars ~ nl => chars.mkString(" ").trim }
  def line: Parser[String] = rep1sep(word, " ") ~ newline ^^ { case c ~ n1 => c.mkString(" ") }
  def word: Parser[String] = rep1(contentChar) ^^ { case chars => chars.mkString }
  def contentChar: Parser[Any] = """[\w\.,;'-]""".r
  def rawText: Parser[String] = rep1(raw) ^^ { case text => text.mkString.trim }
  def raw: Parser[Any] = """.""".r | whiteSpace
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}
