import scala.xml.Null
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.MetaData
import scala.xml.Node

object MarkupModel {
  sealed abstract class Markup(val children: List[Markup]) { def tag = getClass.getSimpleName.toLowerCase }
  case class TextMarkup(content: String) extends Markup(Nil)
  case class Body(items: List[Markup]) extends Markup(items)
  case class BlockQuote(items: List[Markup]) extends Markup(items)
  case class Ol(items: List[Li]) extends Markup(items)
  case class Ul(items: List[Li]) extends Markup(items)
  case class Li(items: List[Markup]) extends Markup(items)
  case class Pre(text: String) extends Markup(List(TextMarkup(text)))
  case class H(level: Int, body: List[Markup]) extends Markup(body) { override def tag = super.tag + level }
  case class P(items: List[Markup]) extends Markup(items)
  case class Tagged(name: String, body: List[Markup]) extends Markup(body) { override def tag = name }
  case class LinkSimple(text: String) extends Markup(List(TextMarkup(text))) { override def tag = "link" }
  case class Link(text: String, key: Key) extends Markup(List(TextMarkup(text), key))
  case class Key(text: String) extends Markup(List(TextMarkup(text)))
  case class LinkDef(link: LinkSimple, url: Url) extends Markup(List(link, url)) {override def tag = "link_def" }
  case class Url(text: String) extends Markup(List(TextMarkup(text)))
}