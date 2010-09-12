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

  /* A sample XML back-end that explicitly defines mappings for some elements: */
  def toXmlSample(elem: Markup): Node = elem match {
    case TextMarkup(text) => Text(text)
    case Body(items) => <body>{ items.map(toXmlSample) }</body>
    case Pre(text) => <pre>{ text }</pre>
    // any other cases that should be handled explicitly can go here
    case e@_ => Elem(null, e.tag, Null, xml.TopScope, e.children.map(toXmlSample):_*)
  }

}