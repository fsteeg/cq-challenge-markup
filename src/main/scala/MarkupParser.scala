import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Node

class MarkupParser extends JavaTokenParsers with RegexParsers {
  override def skipWhitespace = false
  def markup: Parser[Body] = body ^^ { case b => Body(b)}
  def body: Parser[List[Element]] = rep(para|blankline) ^^ { 
    case children => children.filter(_.isInstanceOf[Element]).map(_.asInstanceOf[Element])
  }
  def para: Parser[Para] = text ~ newline ^^ {case c~n => Para(List(c))}
  def text: Parser[Text] = rep1(contentChar) ^^ {case chars => Text(chars.mkString.trim)}
  def blankline: Parser[String] = newline ^^ { case n => "[blank]" }
  def line: Parser[String] = text ~ newline ^^ { case c ~ n1 => c.toString }
  def contentChar: Parser[Any] = """[a-z]""".r | " "
  def newline: Parser[Any] = "\u000D\u000A" | "\u000D" | "\u000A"
}

abstract class Element(children:List[Element]) {
  override def toString = this match {
    case Text(c) => "\"%s\"".format(c)
    case _ => "(:%s %s)".format(getClass.getSimpleName, children.mkString(" "))
  }
  def toXml:Node
}

case class Body(children:List[Element]) extends Element(children) {
  override def toXml = <body>{children.map(_.toXml)}</body>
}
case class Para(children:List[Element]) extends Element(children) {
  override def toXml = <p>{children.map(_.toXml)}</p>
}
case class Text(content:String) extends Element(Nil){
  override def toXml = new scala.xml.Text(content)
}