import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser extends MarkupLexer {

  def body: Parser[Body] = rep(pre | h | p | list | blockquote) ^^ { case c => Body(c) }

  def h: Parser[H] = rep1("*") ~ " " ~ para ^^ { case h ~ s ~ t => H(h.size, t) }
  def p: Parser[P] = para ~ opt(newLine) ^^ { case c ~ n => P(List(TextMarkup(c))) }
  
  def pre: Parser[Pre] = block(3) ^^ { case b => Pre(b) }

  def list: Parser[Markup] = block(2) ^^ {
    case b => {
      val bc: Seq[Char] = b
      bc match {
        case Seq('#', _*) => Ol(parseInternal(ols, b))
        case Seq('-', _*) => Ul(parseInternal(uls, b))
        case _ => BlockQuote(parseInternal(body, b).children)
      }
    }
  }

  def ols: Parser[List[Li]] = rep1("# " ~ li) ^^ { case list => list.map(_ match { case s ~ i => i }) }
  def uls: Parser[List[Li]] = rep1("- " ~ li) ^^ { case list => list.map(_ match { case s ~ i => i }) }

  def li: Parser[Li] = line ~ opt(newLine) ~ opt(block(2)) ~ opt(newLine) ^^ {
    case first ~ _ ~ None ~ _ => Li(parseInternal(body, first).children)
    case first ~ _ ~ rest ~ _ => Li(parseInternal(body, first + "\n\n" + rest.get).children)
  }

  def blockquote: Parser[BlockQuote] = block(2) ^^ {
    case b => BlockQuote(parseInternal(body, b).children)
  }

  def block(n: Int): Parser[String] = rep1(repN(n, " ") ~ rawLine ~ opt(newLine) ~ opt(newLine)) ^^ {
    case text => {
      text.map(_ match {
        case blanks ~ content ~ None ~ _ => content
        case blanks ~ content ~ nl ~ _ => content + "\n"
      }).mkString("\n")
    }
  }

  private def parseInternal[T](e: Parser[T], s: String): T =
    super.parseAll(e, if (s.endsWith("\n")) s else s + "\n") match {
      case Success(result, _) => result
      case no@_ => throw new IllegalStateException(no.toString)
    }

}

class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def para: Parser[String] = rep1(line) ~ opt(newLine) ^^ { case chars ~ _ => chars.mkString(" ").trim }
  def line: Parser[String] = rep1sep(word, " ") ~ newLine ^^ { case c ~ n1 => c.mkString(" ") }
  def word: Parser[String] = rep1(contentChar) ^^ { case chars => chars.mkString }
  def contentChar: Parser[Any] = """[\w\.,;'-<>&]""".r
  def newLine: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
  def rawLine: Parser[String] = rep1(raw) ~ newLine ^^ { case c ~ n1 => c.mkString }
  def raw: Parser[Any] = """.""".r
}
