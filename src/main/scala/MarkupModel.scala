import scala.xml.Null
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.MetaData
import scala.xml.Node

object MarkupModel {
  sealed abstract class Element(children: List[Element])
  case class Body(children: List[Element]) extends Element(children)
  case class Verb(content: Text) extends Element(Nil)
  case class Block(children: List[Element]) extends Element(children)
  case class Header(level: Int, content: Text) extends Element(List(content))
  case class Para(children: List[Element]) extends Element(children) 
  case class Text(content: String) extends Element(Nil)
}