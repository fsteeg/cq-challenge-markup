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
  case class TextMarkup(text: String) extends Element(Nil)
  case class Body(content: List[Element]) extends Element(content)
  case class BlockQuote(content: List[Element]) extends Element(content)
  case class Ol(content: List[Li]) extends Element(content)
  case class Ul(content: List[Li]) extends Element(content)
  case class Li(content: List[Element]) extends Element(content)
  case class Pre(text: String) extends Element(List(TextMarkup(text)))
  case class P(content: List[Element]) extends Element(content)
  case class Link(content: List[Element]) extends Element(content)
  case class Key(text: String) extends Element(List(TextMarkup(text)))
  case class Url(text: String) extends Element(List(TextMarkup(text)))
  case class LinkDef(link: Link, url: Url) extends Element(List(link, url)) { override def tag = "link_def" }
  case class H(level: Int, content: List[Element]) extends Element(content) { override def tag = super.tag + level }
  case class Tagged(name: String, content: List[Element]) extends Element(content) { override def tag = name }

  /** Configuration defaults: */
  private[markup] val defaultSubPattern = """note""".r
  private[markup] val defaultPrettyOutput = true
}
