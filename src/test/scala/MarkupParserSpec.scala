import scala.xml.PrettyPrinter
import scala.util.parsing.combinator.Parsers
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MarkupParserSpec extends MarkupParser with Spec with ShouldMatchers {

  describe("The Markup language") {
    it("uses CR (U+000D), CR/LF, (U+000D U+000A), or LF (U+000A) for line termination") {
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000D").getClass }
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000D\u000A").getClass }
      expect(classOf[Success[Any]]) { parseAll(newline, "\u000A").getClass }
    }
  }
  
  val input = 
"""
a first paragraph

and another
"""

  describe("The Markup parser") {
    it("can parse markup input into an internal tree representation") {
      expect(
       Body(List(
           Para(List(
                Text("a first paragraph"))), 
            Para(List(
                Text("and another")))))
        ) {
        parseAll(markup,input).get
      }
    }
    
    it("can display the parsed structure as a concise string representation") {
      expect("""(:Body (:Para "a first paragraph") (:Para "and another"))""") {
        val res = parseAll(markup,input).get
        println(res)
        res.toString
      }
    }
    
    it("can transform the parsed structure to an XML representation") {
      val pretty = new PrettyPrinter(200,2)
      val xml = <body><p>a first paragraph</p><p>and another</p></body>
      expect(pretty format xml) {
        val res = parseAll(markup,input).get
        println(pretty format res.toXml)
        pretty format res.toXml
      }
    }
    
  }
}