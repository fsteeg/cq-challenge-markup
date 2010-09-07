import scala.xml.Text
import scala.xml.Elem
import scala.xml.Null
import scala.xml.Node
import MarkupModel._

object MarkupBackend {
  def toXml(elem: Markup): Node = elem match {
    case body: Body => <body>{ body.items.map(toXml) }</body>
    case pre: Pre => <pre>{ pre.text }</pre>
    case blockquote: BlockQuote => <blockquote>{ blockquote.items.map(toXml) }</blockquote>
    case h: H => Elem(null, h.tag, Null, xml.TopScope, Text(h.text))
    case p: P => <p>{ p.text }</p>
    case ol: Ol => <ol>{ ol.items.map(toXml) }</ol>
    case ul: Ul => <ul>{ ul.items.map(toXml) }</ul>
    case li: Li => <li>{ li.items.map(toXml) }</li>
  }
}