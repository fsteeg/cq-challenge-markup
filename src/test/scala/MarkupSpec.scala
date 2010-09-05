import scala.xml.PrettyPrinter
import scala.util.parsing.combinator.Parsers
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MarkupModel._

@RunWith(classOf[JUnitRunner])
class MarkupParserSpec extends MarkupParser with Spec with ShouldMatchers {

  val input = """
* the first header

a first paragraph

  some famous words

** the second header

and another
"""

  describe("The Markup language") {
    it("uses CR (U+000D), CR/LF, (U+000D U+000A), or LF (U+000A) for line termination") {
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000D").getClass }
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000D\u000A").getClass }
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000A").getClass }
    }
  }

  describe("The Markup parser") {
    it("can parse markup input into an internal tree representation") {
      expect(
        Body(List(
          Header(1, Text("the first header")),
          Para(List(Text("a first paragraph"))),
          Block(List(
              Para(List(Text("some famous words"))))),
          Header(2, Text("the second header")),
          Para(List(Text("and another")))))
        ) {
        parseAll(markup, input).get
      }
    }
  }

  describe("The Markup model") {
    it("can be exported to an XML representation") {
      val pretty = new PrettyPrinter(200, 2)
      val xml = <body>
                  <h1>the first header</h1>
                  <p>a first paragraph</p>
                  <blockquote>
                    <p>some famous words</p>
                  </blockquote>
                  <h2>the second header</h2>
                  <p>and another</p>
                </body>
      expect(pretty format xml) {
        val res = parseAll(markup, input).get
        println(pretty format res.toXml)
        pretty format res.toXml
      }
    }
  }

}