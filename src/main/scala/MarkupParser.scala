import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node
import MarkupModel._

class MarkupParser(sub: Regex = "note".r) extends MarkupLexer {

  def parseMarkup(m: String): Body = checked(parseAll(body, m))

  def body: Parser[Body] = rep(h | pre | list | blockquote | linkDef | p) ^^ { case c => Body(c) }

  def h: Parser[H] = rep1("*") ~ " " ~ para ^^ { case h ~ _ ~ p => H(h.size, p) }

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

  private def ols: Parser[List[Li]] = rep1("# " ~ li) ^^ { case list => list.map(_ match { case _ ~ i => i }) }
  private def uls: Parser[List[Li]] = rep1("- " ~ li) ^^ { case list => list.map(_ match { case _ ~ i => i }) }

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
        case _ ~ content ~ None ~ _ => content
        case _ ~ content ~ _ ~ _ => content + "\n"
      }).mkString("\n")
    }
  }

  def p: Parser[P] = para ~ opt(newLine) ^^ { case c ~ _ => P(c) }

  private def para: Parser[List[Markup]] =
    rep1(linkWithKey | linkSimple | textContent | taggedSubdoc | taggedTextual) ~ opt(newLine) ^^ {
      case textSections ~ _ => textSections.map(_ match {
        case t: String => TextMarkup(t)
        case sub: Markup => sub
      })
    }

  def linkWithKey: Parser[Link] = "[" ~ tagName ~ "|" ~ tagName ~ "]" ^^ {
    case _ ~ t ~ _ ~ k ~ _ => Link(t, Key(k))
  }

  def linkSimple: Parser[LinkSimple] = "[" ~ tagName ~ "]" ^^ { case _ ~ t ~ _ => LinkSimple(t) }

  def linkDef: Parser[LinkDef] =
    linkSimple ~ rep(" ") ~ "<" ~ url ~ ">" ~ opt(newLine) ~ opt(newLine) ^^ {
      case link ~ _ ~ _ ~ url ~ _ ~ _ ~ _ => LinkDef(link, Url(url))
    }

  def url: Parser[String] = rep1("""[^<>]""".r) ^^ { case url => url.mkString }

  private def taggedTextual: Parser[Tagged] = tagged(tagName, para)
  private def taggedSubdoc: Parser[Tagged] = tagged(sub, subdoc)
  private def subdoc: Parser[List[Markup]] = body ^^ { case c => c.children }
  private def tagged(tag: Parser[String], content: Parser[List[Markup]]) =
    """\""" ~ tag ~ "{" ~ content ~ "}" ^^ { case _ ~ tag ~ _ ~ c ~ _ => Tagged(tag, c) }

  private def parseInternal[T](e: Parser[T], s: String): T = checked(super.parseAll(e, s + "\n"))

  def checked[T](p: ParseResult[T]): T = p match {
    case Success(result, _) => result
    case no@_ => throw new IllegalArgumentException(no.toString)
  }
}

class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def rawLine = rawText ~ newLine ^^ { case c ~ _ => c.mkString }
  def rawPara = rep1sep(rawText, newLine) ~ newLine ^^ { case raw ~ _ => raw.mkString(" ") }
  def rawText = rep1(rawChar) ^^ { case c => c.mkString }
  def rawChar = """.""".r
  def textContent = rep1sep(textWord, newLine) ~ opt(newLine) ^^ { case c ~ _ => c.mkString(" ") }
  def textPara = rep1(textLine) ~ opt(newLine) ^^ { case chars ~ _ => chars.mkString(" ").trim }
  def textLine = textWord ~ newLine ^^ { case c ~ _ => c.mkString }
  def textWord = rep1(textChar) ^^ { case chars => chars.mkString }
  def textChar = """[^\\{}\[\]\n]""".r | """\""" ~ escapedChar ^^ { case _ ~ c => c }
  def escapedChar = requiredEscapes | optionalEscapes
  def requiredEscapes = """\""" | "{" | "}" | "[" | "]"
  def optionalEscapes = "*" | "-" | "#"
  def tagName = rep1("""[\d\w-.+]""".r) ^^ { case chars => chars.mkString }
  def newLine = "\u000D\u000A" | "\u000D" | "\u000A"
}
