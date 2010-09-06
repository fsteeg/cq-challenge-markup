import scala.xml.Elem
import scala.xml.Null
import scala.xml.Node
import MarkupModel._

object MarkupBackend {
  def toXml(elem:Element):Node = elem match {
    case b:Body => <body>{ b.children.map(toXml) }</body>
    case v:Verb => <pre>{ toXml(v.content) }</pre>
    case b:Block => <blockquote>{ b.children.map(toXml) }</blockquote>
    case h:Header => Elem(null, "h" + h.level, Null, xml.TopScope, toXml(h.content))
    case p:Para => <p>{ p.children.map(toXml) }</p>
    case t:Text => new scala.xml.Text(t.content)
  }
}