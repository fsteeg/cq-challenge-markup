import scala.util.parsing.combinator.Parsers
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MarkupParserSpec extends MarkupParser with Spec with ShouldMatchers {
  
  describe("The Markup language") {
    it("uses CR (U+000D), CR/LF, (U+000D U+000A), or LF (U+000A) for line termination"){
      expect(classOf[Success[Any]]){ parseAll(newline, "\u000D").getClass }
      expect(classOf[Success[Any]]){ parseAll(newline, "\u000D\u000A").getClass }
      expect(classOf[Success[Any]]){ parseAll(newline, "\u000A").getClass }
    }
  }
  
  describe("The Markup parser") {
    it("can parse markup input into a tree structure") {
      expect(
          List(
              None, 
              Some("first"), 
              Some("second"), 
              None, 
              Some("more")
              ).toString
      ){parseAll(markup,
"""
first
second

more
"""
      ).get.toString
    }}
  }
}