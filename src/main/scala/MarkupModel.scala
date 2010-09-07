import scala.xml.Null
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.MetaData
import scala.xml.Node

object MarkupModel {
  sealed abstract class Markup(children: List[Markup]) { def tag = getClass.getSimpleName.toLowerCase }
  sealed abstract class Text(content: String) extends Markup(Nil)
  case class Body(items: List[Markup]) extends Markup(items)
  case class BlockQuote(items: List[Markup]) extends Markup(items)
  case class Ol(items: List[Li]) extends Markup(items)
  case class Ul(items: List[Li]) extends Markup(items)
  case class Li(items: List[P]) extends Markup(items)
  case class Pre(text: String) extends Text(text)
  case class H(level: Int, text: String) extends Text(text) { override def tag = super.tag + level }
  case class P(text: String) extends Text(text)
}