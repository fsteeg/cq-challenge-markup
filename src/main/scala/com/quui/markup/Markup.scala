package com.quui.markup

import scala.util.matching.Regex
import scala.xml._
import scala.io.Source._

/**
 * A markup processor, see [[http://codequarterly.net/code-challenges/markup/]]
 * @author Fabian Steeg (fsteeg)
 */
object Markup {

  /**
   * Command-line entry point to use the processor to transform markup to XML.
   * @param args The name of the markup text file to parse and output as XML
   */
  def main(args: Array[String]): Unit = println {
    if (args.size != 1) "Pass a single argument: the name of the markup file to process"
    else toXml(parse(fromFile(args(0)).mkString))
  }

  /**
   * Parse markup.
   * @param input The markup text to parse
   * @param sub ''Optional:'' the pattern describing tag names of markup to be parsed as sub-documents 
   * @return A tree representation of the given input
   */
  def parse(input: String, sub: Regex = defaultSubPattern): Body =
    new MarkupParser(sub).parseMarkup(input)

  /**
   * Export parsed markup to XML.
   * @param markup The root element to export to XML
   * @param pretty ''Optional:'' if true, pretty print the XML
   * @return An XML representation of the given element
   */
  def toXml(markup: Element, pretty: Boolean = defaultPrettyOutput): String = {
    val xml = MarkupBackend.toXml(markup)
    if (pretty) new PrettyPrinter(200, 2).format(xml) else xml.toString
  }

  /**
   * Abstract class for elements a markup tree is made of: an element with child elements and a tag.
   * Concrete elements a markup tree is made of can be used with pattern matching to process a 
   * parsed tree. Some elements with non-standard or variable tag names override the tag def.
   */
  sealed abstract class Element(val children: List[Element]) { def tag = getClass.getSimpleName.toLowerCase }
  case class TextElement(text: String) extends Element(Nil)
  case class Body(content: List[Element]) extends Element(content)
  case class BlockQuote(content: List[Element]) extends Element(content)
  case class Ol(content: List[Li]) extends Element(content)
  case class Ul(content: List[Li]) extends Element(content)
  case class Li(content: List[Element]) extends Element(content)
  case class Pre(text: String) extends Element(List(TextElement(text)))
  case class P(content: List[Element]) extends Element(content)
  case class Link(content: List[Element]) extends Element(content)
  case class Key(text: String) extends Element(List(TextElement(text)))
  case class Url(text: String) extends Element(List(TextElement(text)))
  case class LinkDef(link: Link, url: Url) extends Element(List(link, url)) { override def tag = "link_def" }
  case class H(level: Int, content: List[Element]) extends Element(content) { override def tag = super.tag + level }
  case class Tagged(name: String, content: List[Element]) extends Element(content) { override def tag = name }

  /* Configuration defaults: */
  private[markup] val defaultSubPattern = """note""".r
  private[markup] val defaultPrettyOutput = true
}

import com.quui.markup.Markup._

private[markup] object MarkupBackend {

  /* The simple XML back-end according to the specification: */
  def toXml(elem: Element): Node = elem match {
    case TextElement(text) => Text(text)
    case m@_ => Elem(null, m.tag, Null, xml.TopScope, m.children.map(toXml): _*)
  }

  /* A sample XML back-end that explicitly defines mappings for some elements: */
  def toXmlSample(elem: Element): Node = elem match {
    case TextElement(text) => Text(text)
    case Body(items) => <body>{ items.map(toXmlSample) }</body>
    case Pre(text) => <pre>{ text }</pre>
    case LinkDef(link@_, Url(url)) => <link_def>{ toXmlSample(link) }<url>{ url }</url></link_def>
    // any other cases that should be handled explicitly can go here
    case e@_ => Elem(null, e.tag, Null, xml.TopScope, e.children.map(toXmlSample): _*)
  }

}

import scala.util.parsing.combinator._

private[markup] class MarkupParser(sub: Regex = Markup.defaultSubPattern) extends MarkupLexer {

  def parseMarkup(m: String): Body = checked(parseAll(body, stripped(m)))

  def body: Parser[Body] = rep(h | pre | list | blockquote | linkDef | p) ^^ { case c => Body(c) }

  def h: Parser[H] = rep1("*") ~ " " ~ para ^^ { case h ~ _ ~ p => H(h.size, p) }

  def pre: Parser[Pre] = block(3) ^^ { case b => Pre(b) }

  def list: Parser[Element] = block(2) ^^ {
    case b => {
      val bc: Seq[Char] = b
      bc match {
        case Seq('#', _*) => Ol(parseInternal(ols, b))
        case Seq('-', _*) => Ul(parseInternal(uls, b))
        case _ => BlockQuote(parseInternal(body, b).children)
      }
    }
  }

  def ols: Parser[List[Li]] = rep1("# " ~ li) ^^ { case list => list.map(_ match { case _ ~ i => i }) }
  def uls: Parser[List[Li]] = rep1("- " ~ li) ^^ { case list => list.map(_ match { case _ ~ i => i }) }

  def li: Parser[Li] = rawLine ~ opt(newLine) ~ opt(block(2)) ~ opt(newLine) ^^ {
    case first ~ _ ~ None ~ _ => Li(parseInternal(body, first).children)
    case first ~ _ ~ rest ~ _ => Li(parseInternal(body, first + "\n\n" + rest.get).children)
  }

  def blockquote: Parser[BlockQuote] = block(2) ^^ {
    case b => BlockQuote(parseInternal(body, b).children)
  }

  def block(n: Int): Parser[String] = rep1(repN(n, " ") ~ rawLine ~ opt(newLine) ~ opt(newLine)) ^^ {
    case text => {
      text.map(_ match {
        case _ ~ content ~ None ~ _ => content
        case _ ~ content ~ _ ~ _ => content + "\n"
      }).mkString("\n")
    }
  }

  def p: Parser[P] = para ~ opt(newLine) ^^ { case c ~ _ => P(c) }

  def para: Parser[List[Element]] =
    rep1(linkWithKey | linkSimple | textContent | taggedSubdoc | taggedTextual) ~ opt(newLine) ^^ {
      case textSections ~ _ => textSections.map(_ match {
        case t: String => TextElement(t)
        case sub: Element => sub
      })
    }

  def linkWithKey: Parser[Link] = "[" ~ label ~ "|" ~ tagName ~ "]" ^^ {
    case _ ~ t ~ _ ~ k ~ _ => Link(List(t, Key(k)))
  }

  def linkSimple: Parser[Link] = "[" ~ label ~ "]" ^^ { case _ ~ t ~ _ => Link(List(t)) }

  def label: Parser[Element] = taggedTextual | rep1("""[^|\]]""".r) ^^ {
    case chars => TextElement(chars.mkString)
  }

  def linkDef: Parser[LinkDef] =
    linkSimple ~ rep(" ") ~ "<" ~ url ~ ">" ~ opt(newLine) ~ opt(newLine) ^^ {
      case link ~ _ ~ _ ~ url ~ _ ~ _ ~ _ => LinkDef(link, Url(url))
    }

  def url: Parser[String] = rep1("""[^<>]""".r) ^^ { case url => url.mkString }

  def taggedTextual: Parser[Tagged] = tagged(tagName, para)
  def taggedSubdoc: Parser[Tagged] = tagged(sub, subdoc)
  def subdoc: Parser[List[Element]] = body ^^ { case c => c.children }
  def tagged(tag: Parser[String], content: Parser[List[Element]]) =
    """\""" ~ tag ~ "{" ~ content ~ "}" ^^ { case _ ~ tag ~ _ ~ c ~ _ => Tagged(tag, c) }

  def parseInternal[T](e: Parser[T], s: String): T = checked(super.parseAll(e, s + "\n"))

  def checked[T](p: ParseResult[T]): T = p match {
    case Success(result, _) => result
    case no@_ => throw new IllegalArgumentException(no.toString)
  }

  def stripped(s: String) = stripModeLines(stripTrailing(s))
  def stripModeLines(s: String) = """^-\*-.+\n{1,2}""".r.replaceAllIn(s, "")
  def stripTrailing(s: String) = """[\s]+\n$""".r.replaceAllIn(s, "")
}

private[markup] class MarkupLexer extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def rawLine = rawText ~ newLine ^^ { case c ~ _ => c.mkString }
  def rawPara = rep1sep(rawText, newLine) ~ newLine ^^ { case raw ~ _ => raw.mkString(" ") }
  def rawText = rep1(rawChar) ^^ { case c => c.mkString }
  def rawChar = """.""".r
  def textContent = rep1sep(textWord, newLine) ~ opt(newLine) ^^ { case c ~ _ => c.mkString(" ") }
  def textPara = rep1(textLine) ~ opt(newLine) ^^ { case chars ~ _ => chars.mkString(" ").trim }
  def textLine = textWord ~ newLine ^^ { case c ~ _ => c.mkString }
  def textWord = rep1(textChar) ^^ { case chars => chars.mkString }
  def textChar = """[^\\{}\[\n]""".r | """\""" ~ escapedChar ^^ { case _ ~ c => c }
  def escapedChar = requiredEscapes | optionalEscapes
  def requiredEscapes = """\""" | "{" | "}" | "["
  def optionalEscapes = "*" | "-" | "#"
  def tagName = rep1("""[\d\w-.+]""".r) ^^ { case chars => chars.mkString }
  def newLine = "\u000D\u000A" | "\u000D" | "\u000A"
}
