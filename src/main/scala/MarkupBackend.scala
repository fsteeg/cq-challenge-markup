import scala.xml.Text
import scala.xml.Elem
import scala.xml.Null
import scala.xml.Node
import MarkupModel._

object MarkupBackend {

  /* The simple XML back-end according to the specification: */
  def toXml(elem: Markup): Node = elem match {
    case TextMarkup(text) => Text(text)
    case m@_ => Elem(null, m.tag, Null, xml.TopScope, m.children.map(toXml): _*)
  }

  /* A complete sample XML back-end that explicitly defines a mapping for every element: */
  def toXmlSample(elem: Markup): Node = elem match {
    case TextMarkup(text) => Text(text)
    case Body(items) => <body>{ items.map(toXmlSample) }</body>
    case Pre(text) => <pre>{ text }</pre>
    case BlockQuote(items) => <blockquote>{ items.map(toXmlSample) }</blockquote>
    case P(items) => <p>{ items.map(toXmlSample) }</p>
    case Ol(items) => <ol>{ items.map(toXmlSample) }</ol>
    case Ul(items) => <ul>{ items.map(toXmlSample) }</ul>
    case Li(items) => <li>{ items.map(toXmlSample) }</li>
    case h@H(level, text) => Elem(null, h.tag, Null, xml.TopScope, Text(text))
  }

}