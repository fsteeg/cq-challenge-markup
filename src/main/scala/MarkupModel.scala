import scala.xml.Null
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.MetaData
import scala.xml.Node

object MarkupModel {

  abstract class Element(children: List[Element]) { def toXml: Node }

  case class Body(children: List[Element]) extends Element(children) {
    override def toXml = <body>{ children.map(_.toXml) }</body>
  }

  case class Header(level: Int, content: Text) extends Element(List(content)) {
    override def toXml = Elem(null, "h" + level, Null, xml.TopScope, content.toXml)
  }

  case class Para(children: List[Element]) extends Element(children) {
    override def toXml = <p>{ children.map(_.toXml) }</p>
  }

  case class Text(content: String) extends Element(Nil) {
    override def toXml = new scala.xml.Text(content)
  }

}