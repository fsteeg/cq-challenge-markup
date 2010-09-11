import scala.collection.mutable.ListBuffer
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser extends MarkupLexer {

  def body: Parser[Body] = rep(h | pre | list | blockquote | p) ^^ { case c => Body(c) }

  def h: Parser[H] = rep1("*") ~ " " ~ textPara ^^ {
    case h ~ s ~ t => H(h.size, List(TextMarkup(t)))
  }

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

  private def ols: Parser[List[Li]] = rep1("# " ~ li) ^^ { case list => list.map(_ match { case s ~ i => i }) }
  private def uls: Parser[List[Li]] = rep1("- " ~ li) ^^ { case list => list.map(_ match { case s ~ i => i }) }

  private def li: Parser[Li] = rawLine ~ opt(newLine) ~ opt(block(2)) ~ opt(newLine) ^^ {
    case first ~ _ ~ None ~ _ => Li(parseInternal(body, first).children)
    case first ~ _ ~ rest ~ _ => Li(parseInternal(body, first + "\n\n" + rest.get).children)
  }

  def blockquote: Parser[BlockQuote] = block(2) ^^ {
    case b => BlockQuote(parseInternal(body, b).children)
  }
  
  private def block(n: Int): Parser[String] = rep1(repN(n, " ") ~ rawLine ~ opt(newLine) ~ opt(newLine)) ^^ {
    case text => {
      text.map(_ match {
        case blanks ~ content ~ None ~ _ => content
        case blanks ~ content ~ nl ~ _ => content + "\n"
      }).mkString("\n")
    }
  }

  def p: Parser[P] = para ~ opt(newLine) ^^ { case c ~ _ => P(c) }

  private def para: Parser[List[Markup]] = rep1(textContent | taggedSubdoc | taggedTextual) ~ opt(newLine) ^^ {
    case textSections ~ _ => textSections.map(_ match {
      case t: String => TextMarkup(t)
      case sub@Tagged(n, c) => sub
    })
  }

  private def taggedSubdoc: Parser[Tagged] = tagged(subdocTag, subdoc)
  private def taggedTextual: Parser[Tagged] = tagged(tagName, para)
  private def subdoc: Parser[List[Markup]] = body ^^ { case c => c.children }
  private def tagged(tag: Parser[String], content: Parser[List[Markup]]) =
    """\""" ~ tag ~ "{" ~ content ~ "}" ^^ { case _ ~ tag ~ _ ~ c ~ _ => Tagged(tag, c) }

  private def parseInternal[T](e: Parser[T], s: String): T =
    super.parseAll(e, if (s.endsWith("\n")) s else s + "\n") match {
      case Success(result, _) => result
      case no@_ => throw new IllegalStateException(no.toString)
    }
}

class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def rawLine: Parser[String] = rawText ~ newLine ^^ { case c ~ n1 => c.mkString }
  def rawPara: Parser[String] = rep1sep(rawText, newLine) ~ newLine ^^ { case raw ~ _ => raw.mkString(" ") }
  def rawText: Parser[String] = rep1(rawChar) ^^ { case c => c.mkString }
  def rawChar: Parser[Any] = """.""".r
  def textContent: Parser[String] = rep1sep(textWord, newLine) ~ opt(newLine) ^^ { case c ~ _ => c.mkString(" ") }
  def textPara: Parser[String] = rep1(textLine) ~ opt(newLine) ^^ { case chars ~ _ => chars.mkString(" ").trim }
  def textLine: Parser[String] = textWord ~ newLine ^^ { case c ~ n1 => c.mkString }
  def textWord: Parser[String] = rep1(textChar) ^^ { case chars => chars.mkString }
  def textChar: Parser[Any] = """[\w\.,;'-<>& ]""".r
  def subdocTag: Parser[String] = "note"
  def tagName: Parser[String] = rep1("""[\d\w-.+]""".r) ^^ { case chars => chars.mkString }
  def newLine: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}
